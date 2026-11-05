module LordOfTheRings.Treachery.Cards.ScalingLadders (scalingLadders) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (SiegeTrait))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ScalingLadders = ScalingLadders TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
scalingLadders :: TreacheryCard ScalingLadders
scalingLadders = treachery ScalingLadders Cards.scalingLadders

-- | Counts as a Siege attachment with the text: "Each enemy gets +1 threat."
instance HasModifiersFor ScalingLadders where
  getModifiersFor (ScalingLadders a) = case a.placement of
    AttachedTo _ -> do
      modifySelf a [GainTrait SiegeTrait]
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [ThreatModifier 1]
    _ -> pure ()

{- | When Revealed: Attach to the active location, of two the one the first player
chooses (FAQ 1.34); with none it is discarded.

Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place 1
progress on the current quest (the active location taking it first, FAQ 2.02).
-}
instance RunMessage ScalingLadders where
  runMessage msg t@(ScalingLadders attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      actives <- select IsActiveLocation
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theRingGoesSouth.activeLocation"
        causedBy attrs
        targets actives \l -> attachTreachery attrs.treacheryId l
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeProgressOnQuest attrs 1
      pure t
    _ -> ScalingLadders <$> runMessage msg attrs
