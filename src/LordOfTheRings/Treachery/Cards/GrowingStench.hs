module LordOfTheRings.Treachery.Cards.GrowingStench (growingStench) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (getShelob, shelob)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GrowingStench = GrowingStench TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

growingStench :: TreacheryCard GrowingStench
growingStench = treachery GrowingStench Cards.growingStench

instance RunMessage GrowingStench where
  runMessage msg t@(GrowingStench a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      spider <- getShelob
      for_ spider \eid -> dealRevealedCardAsShadow a eid (MkEncounterCard a.cardId $ toCardCode a)
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      for_ defenders \d -> createEffect (CardIdSource cid) d [CannotReady] (UntilShadowCardLeaves eid cid)
      isShelob <- selectAny (EnemyWithId eid <> shelob)
      when isShelob $ ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      -- The printed scan says "resource"; Hall of Beorn's text says "progress".
      -- https://hallofbeorn.com/Images/Cards/Shelob%27s-Lair-Nightmare/Growing-Stench.jpg
      whenM (selectAny $ EnemyWithId eid <> shelob) $ placeResources a eid 1
      pure t
    _ -> GrowingStench <$> runMessage msg a
