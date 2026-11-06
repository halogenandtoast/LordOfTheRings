{- | An ally that has gained the hero card type, as Sword-thain gives it ("Attached
character loses the ally card type and gains the hero card type"): a hero built from the
ally's card, its stats, traits and printed sphere (FAQ: it keeps its printed sphere icon),
that keeps its own text.

The hero has the ally's id. Its text is the ally's, read through the ally its card's module
builds, given the hero's state and marked 'allyAsHero': such an ally is the hero, its target
and source the hero's, so what its text says of itself is said of the hero. Its attrs keep
no state and do nothing; the hero's attrs do. As Arkham's enemy-locations, it is the one
card under both types, and the game finds it as an ally by its id.
-}
module LordOfTheRings.Hero.AllyHero (AllyHero, allyHero) where

import LordOfTheRings.Ally (Ally, AllyAttrs (..), lookupAlly)
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Hero.Runner ()
import LordOfTheRings.Hero.Types
import LordOfTheRings.Hero.Types qualified as HeroTypes
import LordOfTheRings.Id
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude

newtype AllyHero = AllyHero HeroAttrs
  deriving newtype (Show, Eq, ToJSON, Entity)

instance IsHero AllyHero where
  heroAsAlly (AllyHero a) = Just (asAlly a)

allyHero :: CardCode -> HeroCard AllyHero
allyHero = HeroTypes.hero AllyHero . toCardDef

-- | The ally its card's module builds, as the hero now stands.
asAlly :: HeroAttrs -> Ally
asAlly h =
  overAttrs
    ( \a ->
        a
          { allyPlacement = InPlayArea h.heroOwner
          , allyOwner = h.cardOwner
          , allyTokens = h.heroTokens
          , allyExhausted = h.heroExhausted
          , allyCommitted = h.heroCommitted
          , allyAttacking = h.heroAttacking
          , allyFacedown = h.heroFacedown
          , allyPoison = h.heroPoison
          , allyUnconscious = h.heroUnconscious
          , allyAsHero = True
          }
    )
    $ lookupAlly h.heroCardCode h.heroCardId (AllyId $ unHeroId h.heroId) h.heroOwner

instance HasAbilities AllyHero where
  getAbilities (AllyHero a) = getAbilities (asAlly a)

instance HasModifiersFor AllyHero where
  getModifiersFor (AllyHero a) = getModifiersFor (asAlly a)

instance RunMessage AllyHero where
  runMessage msg (AllyHero a) = do
    _ <- runMessage msg (asAlly a)
    AllyHero <$> runMessage msg a
