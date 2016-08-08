module SemVer where

import Text.Trifecta
import Control.Applicative
import Data.Char

-- 24.11, Ex. 1
-- see: http:// semver.org/

{-
9. A pre-release version MAY be denoted by appending a hyphen and a series of
dot separated identifiers immediately following the patch version. Identifiers
MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-]. Identifiers
MUST NOT be empty. Numeric identifiers MUST NOT include leading zeroes.
Pre-release versions have a lower precedence than the associated normal version.
A pre-release version indicates that the version is unstable and might not
satisfy the intended compatibility requirements as denoted by its associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7, 1.0.0-x.7.z.92.

10. Build metadata MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch or pre-release version.
Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-].
Identifiers MUST NOT be empty. Build metadata SHOULD be ignored when determining
version precedence. Thus two versions that differ only in the build metadata,
have the same precedence.
Examples: 1.0.0-alpha+001, 1.0.0+20130313144700, 1.0.0-beta+exp.sha.5114f85.
-}

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
              deriving (Eq, Show)

-- <EarlyExperiments>
parseSemVerWithNoReleaseMetadata :: Parser SemVer
parseSemVerWithNoReleaseMetadata = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return $ SemVer major minor patch [] []

parseSemVerWithNoReleaseMetadata' :: Parser SemVer
parseSemVerWithNoReleaseMetadata' =
  SemVer <$> (integer <* char '.') <*> (integer <* char '.') <*> integer <*> pure [] <*> pure []
-- </EarlyExperiments>

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> (integer <* char '.') <*> (integer <* char '.') <*> integer <*> (parseRelease <|> pure []) <*> (parseMetadata <|> pure [])

parseRelease :: Parser Release
parseRelease = char '-' *> parseNumberOrString `sepBy1` char '.'

parseMetadata :: Parser Release
parseMetadata = char '+' *> parseNumberOrString `sepBy1` char '.'

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = toNumberOrString <$> some alphaNum
  where
    toNumberOrString a
      | all isDigit a = NOSI $ read a
      | otherwise = NOSS a
