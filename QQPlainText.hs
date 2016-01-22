
module QQPlainText ( plaintext
                   ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Simple Quasi Quoter inserting its contents as a String where it is called

plaintext :: QuasiQuoter
plaintext = QuasiQuoter { quoteExp  = quotePlainTextExp
                        , quotePat  = undefined
                        , quoteDec  = undefined
                        , quoteType = undefined
                        }

quotePlainTextExp :: String -> Q Exp
quotePlainTextExp = dataToExpQ (const Nothing)

