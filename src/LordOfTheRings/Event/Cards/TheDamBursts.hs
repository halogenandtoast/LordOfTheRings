module LordOfTheRings.Event.Cards.TheDamBursts (theDamBursts) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ent, Hobbit))

newtype TheDamBursts = TheDamBursts EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Ent trait and another unique
-- character with the Hobbit trait.
theDamBursts :: EventCard TheDamBursts
theDamBursts = eventWith TheDamBursts Cards.theDamBursts \a -> a & criteriaL .~ uniqueCharactersWith a.owner [Ent, Hobbit]

-- | Action: Ready each damaged Ent character you control.
instance RunMessage TheDamBursts where
  runMessage msg e@(TheDamBursts attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let you = PlayerWithId pid
      traverse_ ready =<< damagedCharacters (HeroControlledBy you <> HeroWithTrait Ent) (AllyControlledBy you <> AllyWithTrait Ent)
      pure e
    _ -> TheDamBursts <$> runMessage msg attrs
