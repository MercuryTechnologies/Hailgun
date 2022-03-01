{-# LANGUAGE CPP #-}
module Mail.Hailgun.Domains
    ( getDomains
    , HailgunDomainResponse(..)
    , HailgunDomain(..)
    ) where

#if __GLASGOW_HASKELL__ < 800
import           Control.Applicative
#endif
import           Control.Monad              (mzero)
import           Data.Aeson
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           Mail.Hailgun.Communication
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.MailgunApi
import           Mail.Hailgun.Pagination
import           Network.HTTP.Client        (httpLbs, newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

-- | Make a request to Mailgun for the domains against your account. This is a paginated request so you must specify
-- the pages of results that you wish to get back.
getDomains
   :: HailgunContext -- ^ The context to operate in which specifies which account to get the domains from.
   -> Page -- ^ The page of results that you wish to see returned.
   -> IO (Either HailgunErrorResponse HailgunDomainResponse) -- ^ The IO response which is either an error or the list of domains.
getDomains context page = do
   request <- getRequest url context (toQueryParams . pageToParams $ page)
   response <- httpLbs request =<< newManager tlsManagerSettings
   return $ parseResponse response
   where
      url = mailgunApiPrefix ++ "/domains"

data HailgunDomainResponse = HailgunDomainResponse
   { hdrTotalCount :: Integer
   , hdrItems      :: [HailgunDomain]
   }
   deriving (Show)

instance FromJSON HailgunDomainResponse where
   parseJSON (Object v) = HailgunDomainResponse
      <$> v .: fromString "total_count"
      <*> v .: fromString "items"
   parseJSON _ = mzero

data HailgunDomain = HailgunDomain
   { domainName         :: T.Text
   , domainSmtpLogin    :: String
   , domainSmtpPassword :: String
   , domainCreatedAt    :: HailgunTime
   , domainWildcard     :: Bool
   , domainSpamAction   :: String -- TODO the domain spam action is probably better specified
   }
   deriving(Show)

instance FromJSON HailgunDomain where
   parseJSON (Object v) = HailgunDomain
      <$> v .: fromString "name"
      <*> v .: fromString "smtp_login"
      <*> v .: fromString "smtp_password"
      <*> v .: fromString "created_at"
      <*> v .: fromString "wildcard"
      <*> v .: fromString "spam_action"
   parseJSON _ = mzero
