module LordOfTheRings.Event.Cards.MutualAccord (mutualAccord) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor, Rohan))

newtype MutualAccord = MutualAccord EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Until the end of the phase, each Gondor card in play gains the Rohan trait,
-- and each Rohan card in play gains the Gondor trait.
mutualAccord :: EventCard MutualAccord
mutualAccord = event MutualAccord Cards.mutualAccord

instance RunMessage MutualAccord where
  runMessage msg e@(MutualAccord attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      -- ponytail: the characters in play as it resolves; attachments, and cards entering
      -- play later this phase, keep their own traits
      gondor <- selectCharacters (HeroWithTrait Gondor) (AllyWithTrait Gondor)
      rohan <- selectCharacters (HeroWithTrait Rohan) (AllyWithTrait Rohan)
      for_ gondor \c -> phaseModifier attrs c (GainTrait Rohan)
      for_ rohan \c -> phaseModifier attrs c (GainTrait Gondor)
      pure e
    _ -> MutualAccord <$> runMessage msg attrs
