module LordOfTheRings.Treachery.Cards.RageFilledHim (rageFilledHim) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RageFilledHim = RageFilledHim TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rageFilledHim :: TreacheryCard RageFilledHim
rageFilledHim = treachery RageFilledHim Cards.rageFilledHim

-- | When Revealed: The Mouth of Sauron makes an immediate attack against you. If this
-- attack destroys a character, raise your threat by 4.
-- Shadow: Attacking enemy gets +1 attack (+3 attack instead if it is The Mouth of Sauron).
instance RunMessage RageFilledHim where
  runMessage msg t@(RageFilledHim attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      mouth <- selectOne (EnemyWithCardCode $ toCardCode Enemies.theMouthOfSauron)
      for_ mouth \eid -> do
        immediateAttack you eid
        raise <- capture $ encounterRaisesThreat you 4
        push $ IfAttackDestroyed eid raise
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      mouth <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.theMouthOfSauron))
      enemyGetsAttack attrs eid (if mouth then 3 else 1)
      pure t
    _ -> RageFilledHim <$> runMessage msg attrs
