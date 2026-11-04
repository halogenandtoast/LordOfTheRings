module LordOfTheRings.Location.Cards.ElvenWaveRunner (elvenWaveRunner) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype ElvenWaveRunner = ElvenWaveRunner LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

elvenWaveRunner :: LocationCard ElvenWaveRunner
elvenWaveRunner = location ElvenWaveRunner Cards.elvenWaveRunner

-- | Response: After Elven Wave-runner is added to the victory display, each player draws 1
-- card.
-- Shadow: If attacking enemy is a Raider, discard a card at random from your hand and
-- place 1 resource on attacking enemy.
instance RunMessage ElvenWaveRunner where
  runMessage msg l@(ElvenWaveRunner a) = case msg of
    -- the first player answers an encounter card's response, as it is in the victory
    -- display by then
    AddToVictoryDisplay (isTarget a -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "elvenWaveRunner.respond"
        causedBy a
        labeled "elvenWaveRunner.draw" $ eachPlayer \p -> drawCards a p 1
        labeled "elvenWaveRunner.decline" $ pure ()
      ElvenWaveRunner <$> runMessage msg a
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) do
        discardRandomFromHand pid 1
        placeResources (CardIdSource cid) eid 1
      pure l
    _ -> ElvenWaveRunner <$> runMessage msg a
