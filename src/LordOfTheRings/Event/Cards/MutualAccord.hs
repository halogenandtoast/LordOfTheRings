module LordOfTheRings.Event.Cards.MutualAccord (mutualAccord) where

import LordOfTheRings.Card (HasCardDef (..))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Placement (Placement (Unplaced))
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
      -- the cards in play as it resolves, as a lasting effect reaches no card entering
      -- play after it
      es <- getsGame gameEntities
      let card' :: (Targetable a, HasCardDef a) => a -> (Target, CardDef)
          card' a = (toTarget a, toCardDef a)
          placed :: (Targetable a, HasCardDef a, HasField "placement" a Placement) => [a] -> [(Target, CardDef)]
          placed xs = [card' x | x <- xs, x.placement /= Unplaced]
          cards =
            map card' (toList es.heroes)
              <> map card' (toList es.allies)
              <> placed (toList es.attachments)
              <> placed (toList es.objectives)
              <> placed (toList es.enemies)
              <> placed (toList es.locations)
              <> placed (toList es.treacheries)
      for_ cards \(t, def) -> do
        gondor <- hasTrait Gondor t def
        rohan <- hasTrait Rohan t def
        when gondor $ phaseModifier attrs t (GainTrait Rohan)
        when rohan $ phaseModifier attrs t (GainTrait Gondor)
      pure e
    _ -> MutualAccord <$> runMessage msg attrs
