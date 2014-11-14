#What is the fastest Unboxed Mutable Reference? 
MutableByteBuffer [^*] <br> 
I was profiling a simple loop. Here is a simpler version:

```haskell
countForever ref = forever $ modifyIORef' ref (+1)
```
I counted 4 assembly instructions. <br>
"Addition takes 300 picoseconds, but there is memory access so let's say two nanoseconds," I thought.<br>
To profile, I wrote:
```haskell
loopHundredMillion = replicateM_ 100000000

iorefLoop = do
  ref <- newIORef 0
  loopHundredMillion $ modifyIORef' ref (+1)
```

I expected this to take ~ 1/5 ^th^ of a second. <br>
It took *1 sec*. <br>
Is five hundred million iterations an unrealistic top-line?<br>
As a sanity check I wrote an analogous program in C.

```c
//CountForever.c
int main (int argc, const char** argv)
{
    int ref = 0;
    while (ref < 1000000)
    {
        ref++;
    }
}
```
```bash
$ gcc -O2 CountForever.c -oCountForever && time ./CountForever

real 0m0.002s
user 0m0.001s
sys	 0m0.001s
```

This was faster then I expected: 

> Maybe **time** isn't that accurate when programs are fast?

On IRC [^1]

<pre><code class="irc">
<span class="irc-name-jfischoff">jfischoff</span><span class="irc-tilde"> ~</span> <span class="irc-jfischoff">Is there anyway to make modifyIORef' faster? </span>
<span class="irc-name-jfischoff">jfischoff</span><span class="irc-tilde"> ~ </span><span class="irc-jfischoff">I am surprised that in a second I was only able to update this ref</span>
         <span class="irc-tilde">↪</span> <span class="irc-jfischoff">100 million times: timeout 1000000 $ forever $ modifyIORef' x (1+)</span>
<span class="irc-name-jfischoff">jfischoff</span><span class="irc-tilde"> ~</span> <span class="irc-jfischoff">where as c++ was able to do the same in 4 milliseconds</span>
<span class="irc-name-glguy">glguy</span><span class="irc-tilde">     ~</span> <span class="irc-glguy">c++ was able to do 1 update every 0.04 nanoseconds?</span>
<span class="irc-name-glguy">glguy</span><span class="irc-tilde">     ~</span> <span class="irc-glguy">an update rate of 25 gigahertz?</span>
<span class="irc-name-dv-">dv-</span><span class="irc-tilde">       ~</span> <span class="irc-dv-">gcc probably just <span class="irc-emphasis">replaced it with a constant</span></span>
<span class="irc-name-jfischoff">jfischoff</span><span class="irc-tilde"> ~ </span><span class="irc-jfischoff">dv-: perhaps</span>
<span class="irc-name-glguy">glguy</span><span class="irc-tilde">     ~</span> <span class="irc-glguy">That or C++ unlocks the fast mode of an Intel processor</span>
</code>
</pre>

Burn.
```bash
$ gcc -02 CountForver.c -S
```

```nasm
; CountForver.s
; Notice, there are no jumps or changes to the program counter.
; There is no looping.
_main:                                  
	.cfi_startproc
BB#0:
	pushq	%rbp
Ltmp2:
	.cfi_def_cfa_offset 16
Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp4:
	.cfi_def_cfa_register %rbp
	xorl	%eax, %eax
	popq	%rbp
	ret
	.cfi_endproc
```
dv- was right. The compiler got rid of the loop, I'm assuming, because I wasn't using the result. <br>
I added a **volatile** keyword to prevent optimizations.

```c
//CountForver.c
int main (int argc, const char** argv)
{
    volatile int ref = 0;
    while (ref < 100000000)
    {
        ref++;
    }
}
```
```bash
$ gcc -O2 CountForever.c -oCountForever && time ./CountForever

real 0m0.178s
user 0m0.176s
sys  0m0.001s
```

So C can do *greater* than **500 million** increments per second, but why stop there? What about assembly?

```nasm
; c0Un7f0r3v3r.asm
EXTERN _exit
GLOBAL start

SECTION .data
  align 8
  iterationCount DD 100000000
  result DD 0

SECTION .text
start:
   ; Copy the iteration count to the ecx register
   ; which is used by the loopnz instruction
   mov ecx, [iterationCount]

loopBlock:  
   inc dword [result] ; Increment the value at the address of result
   loopnz loopBlock   ; Decrement the ecx counter and loop until ecx is zero

exitBlock:
   push dword 0 ; Set the exit code to zero
   mov  eax, 1  ; Place the system call number (exit) in the eax reg
   sub  esp, 4  ; I have to add some dummy space to the stack for some reason
   int  0x80    ; Exit
```
```bash
$ nasm -fmacho c0n7f0r3v3r.asm &&\
> ld -macosx_version_min 10.7.0 -oc0n7f0r3v3r c0n7f0r3v3r.o &&\
> time ./c0Un7f0r3v3r;

real 0m0.176s
user 0m0.174s
sys  0m0.001s
```

So IORef is about *five times slower* than naive approaches in C and assembly. <br>
What gives? <br>
To the Core!

```bash
$ ghc-core -- -O2 IORefLoop.hs
```

```ghc-core
...
case readMutVar# @ RealWorld @ Int ref rwState0 of
    _ { (# rwState1, value #) ->
case value of
    _ { I# unboxedIntValue ->
case writeMutVar# @ RealWorld @ Int ref (I# (unboxedIntValue +# 1)) rwState1 of ...
```
I find GHC Core almost unreadable. <br>
One trick is to try to ignore most of the case statments.<br>
The first and the third case statements are not for scrutinizing alternatives, but are to ensure proper sequencing of IO actions. <br>

<pre class="ghc-core">
<code class="ghc-core">
<span class="state-token-case">case readMutVar# @ RealWorld @ Int ref rwState0 of
    _ { (# rwState1, value #) -></span>
case value of
    _ { I# unboxedIntValue ->
<span class="state-token-case">case writeMutVar# @ RealWorld @ Int ref (I# (unboxedIntValue +# 1)) rwState1 of ..</span>
</code>
</pre>

The second case statement is unboxed a primitive int.

<pre class="ghc-core">
case value of _ { <span class="ghc-core-boxing">I# unboxedIntValue</span> }
</pre>
and the boxing when setting

<pre class="ghc-core">
case writeMutVar# @ RealWorld @ Int ref (<span class="ghc-core-boxing">I# (unboxedIntValue +# 1)</span>)
</pre>

`I#` is the `Int` constructor (the `#` means it is a compiler primitive). It wraps or boxes an unpacked, unboxed, "real" int. <br>
Most of the time the compiler can perform the unboxing automatically, but it can't in this case. [^2] <br>
If it is just boxing then we need a unboxed mutable reference. I can think of two options [Ptr Int](https://hackage.haskell.org/package/base-4.7.0.1/docs/Foreign-Ptr.html) and [MutableByteArray](http://hackage.haskell.org/package/primitive-0.5.3.0/docs/Data-Primitive-ByteArray.html#t:MutableByteArray). <br>

<blockquote class="twitter-tweet tw-align-center" lang="en"><p>Dropping truth bombs.&#10;&#10;“<a href="https://twitter.com/jfischoff">@jfischoff</a>: <a href="https://twitter.com/EvilHaskellTips">@EvilHaskellTips</a> IORefs? No, no, no. Ptrs + Storable. C is the only fast language.”</p>&mdash; Evil Haskell Tips (@EvilHaskellTips) <a href="https://twitter.com/EvilHaskellTips/status/430561860794843136">February 4, 2014</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

First the `Ptr`{.haskell} version

```haskell
ptrHertz = alloca $ \ptr -> do
   poke ptr (0 :: Int)
   loopHundredMillion $ do
     i <- peek ptr
     let !i' = i + 1
     poke ptr i'
```

Using criterion I can see this gives me **0.230 secs**. <br>
Closer. <br>
Now the `MutableByteBuffer`{.haskell} version:

```haskell
mbaHertz = do  
  mba <- newAlignedPinnedByteArray 4 8
  loopHundredMillion $ do
      i <- readByteArray mba 0 :: IO Int
      let !i' = i + 1
      writeByteArray mba 0 i'
```

**0.189** seconds. <br>
About ***5 percent*** slower then C and assembly versions. Nice. <br>
MutableByteBuffers are a length followed by the data immediately. The length is unnecessary in our case, so we are using more space then necessary. <br>
Maybe the extra length is causing a slowdown? <br>
Having a unboxed mutable value like MutableByteBuffer would be nice, too bad we don't have one ... or do we? <br>
There is [MutVar](http://hackage.haskell.org/package/primitive-0.5.3.0/docs/Data-Primitive-MutVar.html), so how does it compare? <br>
Let's compare `MutVar`{.haskell}
```haskell
mutVarHertz = do
  mv <- newMutVar 1
  loopHundredMillion $ do
      i <- readMutVar mv :: IO Int
      let !i' = i + 1
      writeMutVar mv i'
```

Terrible.<br>
There we have it, MutableByteBuffer FTW. <br>
Except I don't really want to use a MutableByteBuffer. It is not type safe, I had to essential cast the data when reading it. It is less type safe then the C version. <br> 
If only there was a safe wrapper around MutableByteArray? There is!  [Vector](https://hackage.haskell.org/package/vector-0.10.11.0/docs/Data-Vector-Mutable.html). 

So does how does `Vector`{.haskell} fair

TODO put in Vector code

TODO profile MutableArray

TODO add UArray code

[Sources](https://github.com/jfischoff/mutable-reference-benchmarks)

# Criterion Reports

## All versions

## Just the fast versions

## The single operations - this really shows off the accuracy of Criterion.


[^1]: <http://ircbrowse.net/browse/haskell?id=18867462&timestamp=1408726262#t1408726262>
[^2]:
<http://ircbrowse.net/browse/haskell?id=18867577&timestamp=1408727702#t1408727702>
[^*]: Fastest on my computer for updating a single unboxed Int