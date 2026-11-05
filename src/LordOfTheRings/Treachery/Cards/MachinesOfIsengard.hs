module LordOfTheRings.Treachery.Cards.MachinesOfIsengard (machinesOfIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (hasWizardry, wizardry)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MachinesOfIsengard = MachinesOfIsengard TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

machinesOfIsengard :: TreacheryCard MachinesOfIsengard
machinesOfIsengard = treachery MachinesOfIsengard Cards.machinesOfIsengard

{- | When Revealed: Trigger the Wizardry effect on the active location. If no Wizardry effect
is triggered this way, Machines of Isengard gains surge. An active location whose text
box is blank has no Wizardry effect to trigger.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, discard 1
random card from your hand.
-}
instance RunMessage MachinesOfIsengard where
  runMessage msg t@(MachinesOfIsengard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      actives <- traverse getLocation =<< select IsActiveLocation
      triggered <- flip filterM actives \l -> (hasWizardry (toCardCode l) &&) . notElem Blank <$> getModifiers l
      players <- getRemainingPlayers
      for_ triggered \l -> wizardry attrs (toCardCode l) players
      when (null triggered) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure t
    _ -> MachinesOfIsengard <$> runMessage msg attrs
