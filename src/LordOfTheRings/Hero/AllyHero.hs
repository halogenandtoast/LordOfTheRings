{- | An ally that has gained the hero card type, as Sword-thain gives it ("Attached
character loses the ally card type and gains the hero card type"): a hero built from the
ally's card, its stats, traits and printed sphere (FAQ: it keeps its printed sphere icon),
that keeps its own text.

Its text is written for an ally, so it is read through the ally the card's module builds,
given the hero's state, and what that text says of the ally is said of the hero: see
'AsHero'.
-}
module LordOfTheRings.Hero.AllyHero (AllyHero, allyHero, AsHero (..)) where

import Control.Monad.Writer.Class (censor)
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (Ability)
import LordOfTheRings.Ally (Ally, AllyAttrs (..), lookupAlly)
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.HasQueue (evalQueueT, pushAll)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Hero.Runner ()
import LordOfTheRings.Hero.Types
import LordOfTheRings.Hero.Types qualified as HeroTypes
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types (AllyMatcher (..), HeroMatcher (..))
import LordOfTheRings.Message (Message)
import LordOfTheRings.Modifier (Modifier (..))
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

newtype AllyHero = AllyHero HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

allyHero :: CardCode -> HeroCard AllyHero
allyHero = HeroTypes.hero AllyHero . toCardDef

-- | The ally its card's module builds, as the hero now stands. It has the hero's id as its
-- own, so what its text says of itself is told apart by the id alone.
innerAlly :: HeroAttrs -> Ally
innerAlly h =
  overAttrs
    ( \a ->
        a
          { allyPlacement = InPlayArea h.heroOwner
          , allyTokens = h.heroTokens
          , allyExhausted = h.heroExhausted
          , allyCommitted = h.heroCommitted
          , allyAttacking = h.heroAttacking
          }
    )
    $ lookupAlly h.heroCardCode h.heroCardId (allyIdOf h.heroId) h.heroOwner

allyIdOf :: HeroId -> AllyId
allyIdOf = AllyId . unHeroId

instance HasAbilities AllyHero where
  getAbilities (AllyHero a) = map (allyAsHero a.heroId) $ getAbilities (innerAlly a)

instance HasModifiersFor AllyHero where
  getModifiersFor (AllyHero a) = censor renamed $ getModifiersFor (innerAlly a)
   where
    renamed (MonoidalMap m) = MonoidalMap $ map (allyAsHero a.heroId) <$> Map.mapKeys (allyAsHero a.heroId) m

{- | The ally's text answers what is said of the hero, said of it, and what it does is
done to the hero. What the ally's attrs alone do, it does not do: the hero's own attrs keep
the hero's state, so the pushes of the ally's attrs, which follow what the card pushed, are
left off.
-}
instance RunMessage AllyHero where
  runMessage msg (AllyHero a) = do
    let inner = innerAlly a
        msg' = heroAsAlly a.heroId msg
    pushed <- lift $ evalQueueT (runMessage msg' inner)
    own <- lift $ evalQueueT (runMessage msg' (toAttrs inner))
    let card = fromMaybe pushed $ stripSuffix own pushed
    pushAll $ map (allyAsHero a.heroId) card
    AllyHero <$> runMessage msg a

{- | What an ally's text says of the ally, said of the hero it has become, and back.

The ally built for the hero has the hero's id, so a reference to it is a reference to the
hero under the other card type. A reference made by a bare 'AllyId', not a target, source or
matcher, is left alone: such a message is an operation only an ally undergoes, as taking
control of an ally or returning it to a hand by its id, and a card that has lost the ally
card type undergoes none of them.
-}
class AsHero a where
  allyAsHero :: HeroId -> a -> a
  heroAsAlly :: HeroId -> a -> a

instance AsHero Target where
  allyAsHero hid = \case
    AllyTarget aid | aid == allyIdOf hid -> HeroTarget hid
    t -> t
  heroAsAlly hid = \case
    HeroTarget h | h == hid -> AllyTarget (allyIdOf hid)
    t -> t

instance AsHero Source where
  allyAsHero hid = \case
    AbilitySource s n -> AbilitySource (allyAsHero hid s) n
    AllySource aid | aid == allyIdOf hid -> HeroSource hid
    s -> s
  heroAsAlly hid = \case
    AbilitySource s n -> AbilitySource (heroAsAlly hid s) n
    HeroSource h | h == hid -> AllySource (allyIdOf hid)
    s -> s

instance AsHero Modifier where
  allyAsHero hid (Modifier s t) = Modifier (allyAsHero hid s) (throughJSON (allyToHero hid) t)
  heroAsAlly hid (Modifier s t) = Modifier (heroAsAlly hid s) (throughJSON (heroToAlly hid) t)

-- | An ability's criteria asking after the ally itself, as "exists (AllyWithId a.id <>
-- ExhaustedAlly)", ask after the hero.
instance AsHero Ability where
  allyAsHero hid = throughJSON (allyToHero hid)
  heroAsAlly hid = throughJSON (heroToAlly hid)

instance AsHero Message where
  allyAsHero hid = throughJSON (allyToHero hid)
  heroAsAlly hid = throughJSON (heroToAlly hid)

{- | Messages, abilities and modifier types are too many constructors deep to rename one
by one, and each is already encoded and decoded whole, as a saved game is; so they are
renamed in their encoding, where a target, source or character id is a tag beside the id.
-}
throughJSON :: (ToJSON a, FromJSON a) => (Value -> Value) -> a -> a
throughJSON f a = case fromJSON (walk (toJSON a)) of
  Success b -> b
  Error _ -> a
 where
  walk = f . \case
    Object o -> Object (walk <$> o)
    Array xs -> Array (walk <$> xs)
    v -> v

allyToHero :: HeroId -> Value -> Value
allyToHero hid = \case
  Object o
    | Just (String "AllyExists") <- KeyMap.lookup "tag" o
    , Just m <- KeyMap.lookup "contents" o
    , Success matcher <- fromJSON m
    , mentionsSelf matcher
    , Just hm <- asHeroMatcher matcher ->
        Object $ KeyMap.insert "tag" (String "HeroExists") $ KeyMap.insert "contents" (toJSON hm) o
  v -> retag [("AllyTarget", "HeroTarget"), ("AllySource", "HeroSource"), ("AllyCharacterId", "HeroCharacterId")] (toJSON hid) v
 where
  self = allyIdOf hid
  mentionsSelf = \case
    AllyWithId aid -> aid == self
    NotAlly m -> mentionsSelf m
    AllyMatchAll ms -> any mentionsSelf ms
    AllyMatchAny ms -> any mentionsSelf ms
    _ -> False
  -- the hero matcher picking out what the ally matcher does, of the hero
  asHeroMatcher = \case
    AllyWithId aid -> HeroWithId hid <$ guard (aid == self)
    AllyControlledBy p -> Just $ HeroControlledBy p
    AllyWithTitle n -> Just $ HeroWithTitle n
    AllyWithTrait t -> Just $ HeroWithTrait t
    AllyWithSphere s -> Just $ HeroWithSphere s
    AllyWithKeyword k -> Just $ HeroWithKeyword k
    ReadyAlly -> Just ReadyHero
    ExhaustedAlly -> Just ExhaustedHero
    CommittedAlly -> Just CommittedHero
    AttackingAlly -> Just AttackingHero
    DefendingAlly -> Just DefendingHero
    -- every hero is unique
    UniqueAlly -> Just AnyHero
    AllyWithAttached t -> Just $ HeroWithAttached t
    AnyAlly -> Just AnyHero
    NotAlly m -> NotHero <$> asHeroMatcher m
    AllyMatchAll ms -> HeroMatchAll <$> traverse asHeroMatcher ms
    AllyMatchAny ms -> HeroMatchAny <$> traverse asHeroMatcher ms
    AllyWithCardCode _ -> Nothing
    AllyEnteredPlayThisRound -> Nothing
    ObjectiveAlly -> Nothing

heroToAlly :: HeroId -> Value -> Value
heroToAlly hid = retag [("HeroTarget", "AllyTarget"), ("HeroSource", "AllySource"), ("HeroCharacterId", "AllyCharacterId")] (toJSON hid)

-- | The tag of a constructor holding just this id, swapped for its counterpart.
retag :: [(Text, Text)] -> Value -> Value -> Value
retag swaps ident = \case
  Object o
    | Just (String t) <- KeyMap.lookup "tag" o
    , Just t' <- lookup t swaps
    , KeyMap.lookup "contents" o == Just ident ->
        Object $ KeyMap.insert "tag" (String t') o
  v -> v
