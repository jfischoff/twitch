module Tests.Twitch where
import Test.Hspec
import qualified Tests.Twitch.Internal     as Internal
import qualified Tests.Twitch.InternalRule as InternalRule
import qualified Tests.Twitch.Main         as Main
import qualified Tests.Twitch.Path         as Path
import qualified Tests.Twitch.Rule         as Rule
import qualified Tests.Twitch.Run          as Run

tests :: Spec 
tests = foldl1 (>>) 
    [ Internal.tests
    , InternalRule.tests
    , Main.tests
    , Path.tests
    , Rule.tests
    , Run.tests 
    ]
