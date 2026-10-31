module LordOfTheRings.Treachery.Cards.ShadowFall (shadowFall) where

import LordOfTheRings.Trait (Trait (Dark))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShadowFall = ShadowFall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowFall :: TreacheryCard ShadowFall
shadowFall = treachery ShadowFall Cards.shadowFall

-- | Counts as a Condition attachment with the text: "Attached location gains the Dark
-- trait and gets +3 quest points."
instance HasModifiersFor ShadowFall where
  getModifiersFor (ShadowFall a) = withHost a \host -> modified a host [GainTrait Dark, QuestPointModifier 3]

-- | When Revealed: Attach to a non-Dark location in play.
-- Shadow: Attacking enemy gets +3 defense this phase.
instance RunMessage ShadowFall where
  runMessage msg t@(ShadowFall attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      locations <- select (not_ $ LocationWithTrait Dark)
      chooseOrRunOneM lead do
        questionLabeled "shadowFall.attach"
        causedBy attrs
        targets locations \l -> attachTreachery attrs.treacheryId l
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 3)
      pure t
    _ -> ShadowFall <$> runMessage msg attrs
