import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withTierList)

main :: IO ()
main = defaultMain fromArgs withTierList