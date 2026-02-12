module Yoga.BetterAuth.SocialProviders
  ( SocialProviders
  , SocialProvidersImpl
  , socialProviders
  , BaseProviderImpl
  , ProviderId
  , SignInSocialOptionsImpl
  , SignInSocialResult
  -- Google
  , GoogleProvider
  , GoogleProviderImpl
  , google
  , googleId
  -- GitHub
  , GithubProvider
  , GithubProviderImpl
  , github
  , githubId
  -- Apple
  , AppleProvider
  , AppleProviderImpl
  , apple
  , appleId
  -- Atlassian
  , AtlassianProvider
  , AtlassianProviderImpl
  , atlassian
  , atlassianId
  -- Cognito
  , CognitoProvider
  , CognitoProviderImpl
  , cognito
  , cognitoId
  -- Discord
  , DiscordProvider
  , DiscordProviderImpl
  , discord
  , discordId
  -- Dropbox
  , DropboxProvider
  , DropboxProviderImpl
  , dropbox
  , dropboxId
  -- Facebook
  , FacebookProvider
  , FacebookProviderImpl
  , facebook
  , facebookId
  -- Figma
  , FigmaProvider
  , FigmaProviderImpl
  , figma
  , figmaId
  -- GitLab
  , GitlabProvider
  , GitlabProviderImpl
  , gitlab
  , gitlabId
  -- HuggingFace
  , HuggingfaceProvider
  , HuggingfaceProviderImpl
  , huggingface
  , huggingfaceId
  -- Kakao
  , KakaoProvider
  , KakaoProviderImpl
  , kakao
  , kakaoId
  -- Kick
  , KickProvider
  , KickProviderImpl
  , kick
  , kickId
  -- Line
  , LineProvider
  , LineProviderImpl
  , line
  , lineId
  -- Linear
  , LinearProvider
  , LinearProviderImpl
  , linear
  , linearId
  -- LinkedIn
  , LinkedinProvider
  , LinkedinProviderImpl
  , linkedin
  , linkedinId
  -- Microsoft
  , MicrosoftProvider
  , MicrosoftProviderImpl
  , microsoft
  , microsoftId
  -- Naver
  , NaverProvider
  , NaverProviderImpl
  , naver
  , naverId
  -- Notion
  , NotionProvider
  , NotionProviderImpl
  , notion
  , notionId
  -- Paybin
  , PaybinProvider
  , PaybinProviderImpl
  , paybin
  , paybinId
  -- PayPal
  , PaypalProvider
  , PaypalProviderImpl
  , paypal
  , paypalId
  -- Polar
  , PolarProvider
  , PolarProviderImpl
  , polar
  , polarId
  -- Reddit
  , RedditProvider
  , RedditProviderImpl
  , reddit
  , redditId
  -- Roblox
  , RobloxProvider
  , RobloxProviderImpl
  , roblox
  , robloxId
  -- Salesforce
  , SalesforceProvider
  , SalesforceProviderImpl
  , salesforce
  , salesforceId
  -- Slack
  , SlackProvider
  , SlackProviderImpl
  , slack
  , slackId
  -- Spotify
  , SpotifyProvider
  , SpotifyProviderImpl
  , spotify
  , spotifyId
  -- TikTok
  , TiktokProvider
  , TiktokProviderImpl
  , tiktok
  , tiktokId
  -- Twitch
  , TwitchProvider
  , TwitchProviderImpl
  , twitch
  , twitchId
  -- Twitter
  , TwitterProvider
  , TwitterProviderImpl
  , twitter
  , twitterId
  -- Vercel
  , VercelProvider
  , VercelProviderImpl
  , vercel
  , vercelId
  -- VK
  , VkProvider
  , VkProviderImpl
  , vk
  , vkId
  -- Zoom
  , ZoomProvider
  , ZoomProviderImpl
  , zoom
  , zoomId
  ) where

import Prelude

import Data.Maybe (Maybe)
import Foreign (Foreign)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

-- | Provider identifier for use with signInSocial
newtype ProviderId = ProviderId String

derive newtype instance Eq ProviderId
derive newtype instance Show ProviderId

-- | Common fields shared by all social provider configurations
type BaseProviderImpl r =
  ( clientId :: String
  , clientSecret :: String
  , redirectURI :: String
  , scope :: Array String
  , disableDefaultScope :: Boolean
  , disableSignUp :: Boolean
  , disableImplicitSignUp :: Boolean
  , overrideUserInfoOnSignIn :: Boolean
  , prompt :: String
  , responseMode :: String
  , enabled :: Boolean
  , disableIdTokenSignIn :: Boolean
  , clientKey :: String
  | r
  )

--------------------------------------------------------------------------------
-- Google
--------------------------------------------------------------------------------

newtype GoogleProvider = GoogleProvider Foreign

type GoogleProviderImpl = BaseProviderImpl
  ( accessType :: String
  , display :: String
  , hd :: String
  )

google :: forall opts opts_. Union opts opts_ GoogleProviderImpl => { | opts } -> GoogleProvider
google opts = GoogleProvider (unsafeCoerce opts)

googleId :: ProviderId
googleId = ProviderId "google"

--------------------------------------------------------------------------------
-- GitHub
--------------------------------------------------------------------------------

newtype GithubProvider = GithubProvider Foreign

type GithubProviderImpl = BaseProviderImpl ()

github :: forall opts opts_. Union opts opts_ GithubProviderImpl => { | opts } -> GithubProvider
github opts = GithubProvider (unsafeCoerce opts)

githubId :: ProviderId
githubId = ProviderId "github"

--------------------------------------------------------------------------------
-- Apple
--------------------------------------------------------------------------------

newtype AppleProvider = AppleProvider Foreign

type AppleProviderImpl = BaseProviderImpl
  ( appBundleIdentifier :: String
  , audience :: String
  )

apple :: forall opts opts_. Union opts opts_ AppleProviderImpl => { | opts } -> AppleProvider
apple opts = AppleProvider (unsafeCoerce opts)

appleId :: ProviderId
appleId = ProviderId "apple"

--------------------------------------------------------------------------------
-- Atlassian
--------------------------------------------------------------------------------

newtype AtlassianProvider = AtlassianProvider Foreign

type AtlassianProviderImpl = BaseProviderImpl ()

atlassian :: forall opts opts_. Union opts opts_ AtlassianProviderImpl => { | opts } -> AtlassianProvider
atlassian opts = AtlassianProvider (unsafeCoerce opts)

atlassianId :: ProviderId
atlassianId = ProviderId "atlassian"

--------------------------------------------------------------------------------
-- Cognito
--------------------------------------------------------------------------------

newtype CognitoProvider = CognitoProvider Foreign

type CognitoProviderImpl = BaseProviderImpl
  ( domain :: String
  , region :: String
  , userPoolId :: String
  , requireClientSecret :: Boolean
  )

cognito :: forall opts opts_. Union opts opts_ CognitoProviderImpl => { | opts } -> CognitoProvider
cognito opts = CognitoProvider (unsafeCoerce opts)

cognitoId :: ProviderId
cognitoId = ProviderId "cognito"

--------------------------------------------------------------------------------
-- Discord
--------------------------------------------------------------------------------

newtype DiscordProvider = DiscordProvider Foreign

type DiscordProviderImpl = BaseProviderImpl
  ( permissions :: Int
  )

discord :: forall opts opts_. Union opts opts_ DiscordProviderImpl => { | opts } -> DiscordProvider
discord opts = DiscordProvider (unsafeCoerce opts)

discordId :: ProviderId
discordId = ProviderId "discord"

--------------------------------------------------------------------------------
-- Dropbox
--------------------------------------------------------------------------------

newtype DropboxProvider = DropboxProvider Foreign

type DropboxProviderImpl = BaseProviderImpl
  ( accessType :: String
  )

dropbox :: forall opts opts_. Union opts opts_ DropboxProviderImpl => { | opts } -> DropboxProvider
dropbox opts = DropboxProvider (unsafeCoerce opts)

dropboxId :: ProviderId
dropboxId = ProviderId "dropbox"

--------------------------------------------------------------------------------
-- Facebook
--------------------------------------------------------------------------------

newtype FacebookProvider = FacebookProvider Foreign

type FacebookProviderImpl = BaseProviderImpl
  ( fields :: Array String
  , configId :: String
  )

facebook :: forall opts opts_. Union opts opts_ FacebookProviderImpl => { | opts } -> FacebookProvider
facebook opts = FacebookProvider (unsafeCoerce opts)

facebookId :: ProviderId
facebookId = ProviderId "facebook"

--------------------------------------------------------------------------------
-- Figma
--------------------------------------------------------------------------------

newtype FigmaProvider = FigmaProvider Foreign

type FigmaProviderImpl = BaseProviderImpl ()

figma :: forall opts opts_. Union opts opts_ FigmaProviderImpl => { | opts } -> FigmaProvider
figma opts = FigmaProvider (unsafeCoerce opts)

figmaId :: ProviderId
figmaId = ProviderId "figma"

--------------------------------------------------------------------------------
-- GitLab
--------------------------------------------------------------------------------

newtype GitlabProvider = GitlabProvider Foreign

type GitlabProviderImpl = BaseProviderImpl
  ( issuer :: String
  )

gitlab :: forall opts opts_. Union opts opts_ GitlabProviderImpl => { | opts } -> GitlabProvider
gitlab opts = GitlabProvider (unsafeCoerce opts)

gitlabId :: ProviderId
gitlabId = ProviderId "gitlab"

--------------------------------------------------------------------------------
-- HuggingFace
--------------------------------------------------------------------------------

newtype HuggingfaceProvider = HuggingfaceProvider Foreign

type HuggingfaceProviderImpl = BaseProviderImpl ()

huggingface :: forall opts opts_. Union opts opts_ HuggingfaceProviderImpl => { | opts } -> HuggingfaceProvider
huggingface opts = HuggingfaceProvider (unsafeCoerce opts)

huggingfaceId :: ProviderId
huggingfaceId = ProviderId "huggingface"

--------------------------------------------------------------------------------
-- Kakao
--------------------------------------------------------------------------------

newtype KakaoProvider = KakaoProvider Foreign

type KakaoProviderImpl = BaseProviderImpl ()

kakao :: forall opts opts_. Union opts opts_ KakaoProviderImpl => { | opts } -> KakaoProvider
kakao opts = KakaoProvider (unsafeCoerce opts)

kakaoId :: ProviderId
kakaoId = ProviderId "kakao"

--------------------------------------------------------------------------------
-- Kick
--------------------------------------------------------------------------------

newtype KickProvider = KickProvider Foreign

type KickProviderImpl = BaseProviderImpl ()

kick :: forall opts opts_. Union opts opts_ KickProviderImpl => { | opts } -> KickProvider
kick opts = KickProvider (unsafeCoerce opts)

kickId :: ProviderId
kickId = ProviderId "kick"

--------------------------------------------------------------------------------
-- Line
--------------------------------------------------------------------------------

newtype LineProvider = LineProvider Foreign

type LineProviderImpl = BaseProviderImpl ()

line :: forall opts opts_. Union opts opts_ LineProviderImpl => { | opts } -> LineProvider
line opts = LineProvider (unsafeCoerce opts)

lineId :: ProviderId
lineId = ProviderId "line"

--------------------------------------------------------------------------------
-- Linear
--------------------------------------------------------------------------------

newtype LinearProvider = LinearProvider Foreign

type LinearProviderImpl = BaseProviderImpl ()

linear :: forall opts opts_. Union opts opts_ LinearProviderImpl => { | opts } -> LinearProvider
linear opts = LinearProvider (unsafeCoerce opts)

linearId :: ProviderId
linearId = ProviderId "linear"

--------------------------------------------------------------------------------
-- LinkedIn
--------------------------------------------------------------------------------

newtype LinkedinProvider = LinkedinProvider Foreign

type LinkedinProviderImpl = BaseProviderImpl ()

linkedin :: forall opts opts_. Union opts opts_ LinkedinProviderImpl => { | opts } -> LinkedinProvider
linkedin opts = LinkedinProvider (unsafeCoerce opts)

linkedinId :: ProviderId
linkedinId = ProviderId "linkedin"

--------------------------------------------------------------------------------
-- Microsoft
--------------------------------------------------------------------------------

newtype MicrosoftProvider = MicrosoftProvider Foreign

type MicrosoftProviderImpl = BaseProviderImpl
  ( tenantId :: String
  , authority :: String
  , profilePhotoSize :: Int
  , disableProfilePhoto :: Boolean
  )

microsoft :: forall opts opts_. Union opts opts_ MicrosoftProviderImpl => { | opts } -> MicrosoftProvider
microsoft opts = MicrosoftProvider (unsafeCoerce opts)

microsoftId :: ProviderId
microsoftId = ProviderId "microsoft"

--------------------------------------------------------------------------------
-- Naver
--------------------------------------------------------------------------------

newtype NaverProvider = NaverProvider Foreign

type NaverProviderImpl = BaseProviderImpl ()

naver :: forall opts opts_. Union opts opts_ NaverProviderImpl => { | opts } -> NaverProvider
naver opts = NaverProvider (unsafeCoerce opts)

naverId :: ProviderId
naverId = ProviderId "naver"

--------------------------------------------------------------------------------
-- Notion
--------------------------------------------------------------------------------

newtype NotionProvider = NotionProvider Foreign

type NotionProviderImpl = BaseProviderImpl ()

notion :: forall opts opts_. Union opts opts_ NotionProviderImpl => { | opts } -> NotionProvider
notion opts = NotionProvider (unsafeCoerce opts)

notionId :: ProviderId
notionId = ProviderId "notion"

--------------------------------------------------------------------------------
-- Paybin
--------------------------------------------------------------------------------

newtype PaybinProvider = PaybinProvider Foreign

type PaybinProviderImpl = BaseProviderImpl
  ( issuer :: String
  )

paybin :: forall opts opts_. Union opts opts_ PaybinProviderImpl => { | opts } -> PaybinProvider
paybin opts = PaybinProvider (unsafeCoerce opts)

paybinId :: ProviderId
paybinId = ProviderId "paybin"

--------------------------------------------------------------------------------
-- PayPal
--------------------------------------------------------------------------------

newtype PaypalProvider = PaypalProvider Foreign

type PaypalProviderImpl = BaseProviderImpl
  ( environment :: String
  , requestShippingAddress :: Boolean
  )

paypal :: forall opts opts_. Union opts opts_ PaypalProviderImpl => { | opts } -> PaypalProvider
paypal opts = PaypalProvider (unsafeCoerce opts)

paypalId :: ProviderId
paypalId = ProviderId "paypal"

--------------------------------------------------------------------------------
-- Polar
--------------------------------------------------------------------------------

newtype PolarProvider = PolarProvider Foreign

type PolarProviderImpl = BaseProviderImpl ()

polar :: forall opts opts_. Union opts opts_ PolarProviderImpl => { | opts } -> PolarProvider
polar opts = PolarProvider (unsafeCoerce opts)

polarId :: ProviderId
polarId = ProviderId "polar"

--------------------------------------------------------------------------------
-- Reddit
--------------------------------------------------------------------------------

newtype RedditProvider = RedditProvider Foreign

type RedditProviderImpl = BaseProviderImpl
  ( duration :: String
  )

reddit :: forall opts opts_. Union opts opts_ RedditProviderImpl => { | opts } -> RedditProvider
reddit opts = RedditProvider (unsafeCoerce opts)

redditId :: ProviderId
redditId = ProviderId "reddit"

--------------------------------------------------------------------------------
-- Roblox
--------------------------------------------------------------------------------

newtype RobloxProvider = RobloxProvider Foreign

type RobloxProviderImpl = BaseProviderImpl ()

roblox :: forall opts opts_. Union opts opts_ RobloxProviderImpl => { | opts } -> RobloxProvider
roblox opts = RobloxProvider (unsafeCoerce opts)

robloxId :: ProviderId
robloxId = ProviderId "roblox"

--------------------------------------------------------------------------------
-- Salesforce
--------------------------------------------------------------------------------

newtype SalesforceProvider = SalesforceProvider Foreign

type SalesforceProviderImpl = BaseProviderImpl
  ( environment :: String
  , loginUrl :: String
  )

salesforce :: forall opts opts_. Union opts opts_ SalesforceProviderImpl => { | opts } -> SalesforceProvider
salesforce opts = SalesforceProvider (unsafeCoerce opts)

salesforceId :: ProviderId
salesforceId = ProviderId "salesforce"

--------------------------------------------------------------------------------
-- Slack
--------------------------------------------------------------------------------

newtype SlackProvider = SlackProvider Foreign

type SlackProviderImpl = BaseProviderImpl ()

slack :: forall opts opts_. Union opts opts_ SlackProviderImpl => { | opts } -> SlackProvider
slack opts = SlackProvider (unsafeCoerce opts)

slackId :: ProviderId
slackId = ProviderId "slack"

--------------------------------------------------------------------------------
-- Spotify
--------------------------------------------------------------------------------

newtype SpotifyProvider = SpotifyProvider Foreign

type SpotifyProviderImpl = BaseProviderImpl ()

spotify :: forall opts opts_. Union opts opts_ SpotifyProviderImpl => { | opts } -> SpotifyProvider
spotify opts = SpotifyProvider (unsafeCoerce opts)

spotifyId :: ProviderId
spotifyId = ProviderId "spotify"

--------------------------------------------------------------------------------
-- TikTok
--------------------------------------------------------------------------------

newtype TiktokProvider = TiktokProvider Foreign

type TiktokProviderImpl =
  ( clientKey :: String
  , clientSecret :: String
  , redirectURI :: String
  , scope :: Array String
  , disableDefaultScope :: Boolean
  , disableSignUp :: Boolean
  , disableImplicitSignUp :: Boolean
  , overrideUserInfoOnSignIn :: Boolean
  , prompt :: String
  , responseMode :: String
  , enabled :: Boolean
  , disableIdTokenSignIn :: Boolean
  )

tiktok :: forall opts opts_. Union opts opts_ TiktokProviderImpl => { | opts } -> TiktokProvider
tiktok opts = TiktokProvider (unsafeCoerce opts)

tiktokId :: ProviderId
tiktokId = ProviderId "tiktok"

--------------------------------------------------------------------------------
-- Twitch
--------------------------------------------------------------------------------

newtype TwitchProvider = TwitchProvider Foreign

type TwitchProviderImpl = BaseProviderImpl
  ( claims :: Array String
  )

twitch :: forall opts opts_. Union opts opts_ TwitchProviderImpl => { | opts } -> TwitchProvider
twitch opts = TwitchProvider (unsafeCoerce opts)

twitchId :: ProviderId
twitchId = ProviderId "twitch"

--------------------------------------------------------------------------------
-- Twitter
--------------------------------------------------------------------------------

newtype TwitterProvider = TwitterProvider Foreign

type TwitterProviderImpl = BaseProviderImpl ()

twitter :: forall opts opts_. Union opts opts_ TwitterProviderImpl => { | opts } -> TwitterProvider
twitter opts = TwitterProvider (unsafeCoerce opts)

twitterId :: ProviderId
twitterId = ProviderId "twitter"

--------------------------------------------------------------------------------
-- Vercel
--------------------------------------------------------------------------------

newtype VercelProvider = VercelProvider Foreign

type VercelProviderImpl = BaseProviderImpl ()

vercel :: forall opts opts_. Union opts opts_ VercelProviderImpl => { | opts } -> VercelProvider
vercel opts = VercelProvider (unsafeCoerce opts)

vercelId :: ProviderId
vercelId = ProviderId "vercel"

--------------------------------------------------------------------------------
-- VK
--------------------------------------------------------------------------------

newtype VkProvider = VkProvider Foreign

type VkProviderImpl = BaseProviderImpl
  ( scheme :: String
  )

vk :: forall opts opts_. Union opts opts_ VkProviderImpl => { | opts } -> VkProvider
vk opts = VkProvider (unsafeCoerce opts)

vkId :: ProviderId
vkId = ProviderId "vk"

--------------------------------------------------------------------------------
-- Zoom
--------------------------------------------------------------------------------

newtype ZoomProvider = ZoomProvider Foreign

type ZoomProviderImpl = BaseProviderImpl
  ( pkce :: Boolean
  )

zoom :: forall opts opts_. Union opts opts_ ZoomProviderImpl => { | opts } -> ZoomProvider
zoom opts = ZoomProvider (unsafeCoerce opts)

zoomId :: ProviderId
zoomId = ProviderId "zoom"

--------------------------------------------------------------------------------
-- SocialProviders record
--------------------------------------------------------------------------------

newtype SocialProviders = SocialProviders Foreign

type SocialProvidersImpl =
  ( google :: GoogleProvider
  , github :: GithubProvider
  , apple :: AppleProvider
  , atlassian :: AtlassianProvider
  , cognito :: CognitoProvider
  , discord :: DiscordProvider
  , dropbox :: DropboxProvider
  , facebook :: FacebookProvider
  , figma :: FigmaProvider
  , gitlab :: GitlabProvider
  , huggingface :: HuggingfaceProvider
  , kakao :: KakaoProvider
  , kick :: KickProvider
  , line :: LineProvider
  , linear :: LinearProvider
  , linkedin :: LinkedinProvider
  , microsoft :: MicrosoftProvider
  , naver :: NaverProvider
  , notion :: NotionProvider
  , paybin :: PaybinProvider
  , paypal :: PaypalProvider
  , polar :: PolarProvider
  , reddit :: RedditProvider
  , roblox :: RobloxProvider
  , salesforce :: SalesforceProvider
  , slack :: SlackProvider
  , spotify :: SpotifyProvider
  , tiktok :: TiktokProvider
  , twitch :: TwitchProvider
  , twitter :: TwitterProvider
  , vercel :: VercelProvider
  , vk :: VkProvider
  , zoom :: ZoomProvider
  )

socialProviders :: forall r r_. Union r r_ SocialProvidersImpl => { | r } -> SocialProviders
socialProviders opts = SocialProviders (unsafeCoerce opts)

--------------------------------------------------------------------------------
-- signInSocial types
--------------------------------------------------------------------------------

type SignInSocialOptionsImpl =
  ( callbackURL :: String
  , newUserCallbackURL :: String
  , errorCallbackURL :: String
  , disableRedirect :: Boolean
  , scopes :: Array String
  , requestSignUp :: Boolean
  , loginHint :: String
  )

type SignInSocialResult =
  { url :: Maybe String
  , redirect :: Boolean
  }
