module LordOfTheRings.Treachery.Cards.InjuredCompanion (injuredCompanion) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InjuredCompanion = InjuredCompanion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

injuredCompanion :: TreacheryCard InjuredCompanion
injuredCompanion = treachery InjuredCompanion Cards.injuredCompanion

-- | When Revealed: Each player must choose: either raise your threat by 1 for each
-- character you control, or discard a character you control.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, raise each
-- player's threat by 1.
instance RunMessage InjuredCompanion where
  runMessage msg t@(InjuredCompanion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        characters <- charactersControlledBy p
        chooseOneM p do
          questionLabeled "injuredCompanion.choose"
          causedBy attrs
          labeledWith "injuredCompanion.raiseThreat" [intVar "n" (length characters)]
            $ encounterRaisesThreat p (length characters)
          unless (null characters) $ labeled "injuredCompanion.discard" do
            chooseOrRunOneM p do
              questionLabeled "injuredCompanion.character"
              causedBy attrs
              targets characters (discard attrs)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure t
    _ -> InjuredCompanion <$> runMessage msg attrs
