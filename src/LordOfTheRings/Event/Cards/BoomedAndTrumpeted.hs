module LordOfTheRings.Event.Cards.BoomedAndTrumpeted (boomedAndTrumpeted) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ent))

newtype BoomedAndTrumpeted = BoomedAndTrumpeted EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an Ent character takes any amount of damage, ready it. That character
-- gets +3 attack until the end of the phase.
boomedAndTrumpeted :: EventCard BoomedAndTrumpeted
boomedAndTrumpeted =
  eventWith BoomedAndTrumpeted Cards.boomedAndTrumpeted (windowL ?~ CharacterWithTraitDealtDamageWindow After Ent)

instance RunMessage BoomedAndTrumpeted where
  runMessage msg e@(BoomedAndTrumpeted attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      ents <- selectCharacters (HeroWithTrait Ent) (AllyWithTrait Ent)
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy attrs
        targets (nub [t | Window _ (DealtDamage t _) <- windows, t `elem` ents]) \t -> do
          ready t
          phaseModifier attrs t (AttackModifier 3)
      pure e
    _ -> BoomedAndTrumpeted <$> runMessage msg attrs
