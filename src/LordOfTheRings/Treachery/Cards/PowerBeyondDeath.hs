module LordOfTheRings.Treachery.Cards.PowerBeyondDeath (powerBeyondDeath) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PowerBeyondDeath = PowerBeyondDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

powerBeyondDeath :: TreacheryCard PowerBeyondDeath
powerBeyondDeath = treachery PowerBeyondDeath Cards.powerBeyondDeath

-- | Counts as a Condition attachment with the text: "Daechanar gets +2 defense."
instance HasModifiersFor PowerBeyondDeath where
  getModifiersFor (PowerBeyondDeath a) = case a.placement of
    AttachedTo _ -> do
      daechanar <- select (EnemyWithCardCode $ toCardCode Enemies.daechanar)
      for_ daechanar \d -> modified a d [DefenseModifier 2]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to the main quest.
-- Shadow: If this attack destroys a character, resolve this card's 'when revealed' effect.
instance RunMessage PowerBeyondDeath where
  runMessage msg t@(PowerBeyondDeath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) . headMay =<< getMainQuests
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      traverse_ (attachShadowTreachery eid cid) . headMay =<< getMainQuests
      pure t
    _ -> PowerBeyondDeath <$> runMessage msg attrs
