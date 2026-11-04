module LordOfTheRings.Treachery.Cards.SauronsAgent (sauronsAgent) where

import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (getBellach, placeDamageOnActiveLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SauronsAgent = SauronsAgent TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sauronsAgent :: TreacheryCard SauronsAgent
sauronsAgent = treachery SauronsAgent Cards.sauronsAgent

-- | When Revealed: Bellach makes an immediate attack against each player in turn order.
-- Then, deal 1 damage to the active location for each character destroyed by this effect.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage SauronsAgent where
  runMessage msg t@(SauronsAgent attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getBellach >>= traverse_ \bellach -> attackNext 0 bellach =<< getRemainingPlayers
      pure t
    -- after each attack, what it destroyed is added to the count, the players still to be
    -- attacked carried in the step
    DoStep destroyed (Run (WhenRevealed (isTarget attrs -> True) : EnemyAttack _ bellach : rest)) -> do
      n <- length <$> attackDestroyed bellach
      remaining <- getRemainingPlayers
      attackNext (destroyed + n) bellach [p | EnemyAttack p _ <- rest, p `elem` remaining]
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> SauronsAgent <$> runMessage msg attrs
   where
    attackNext destroyed bellach = \case
      [] -> when (destroyed > 0) $ placeDamageOnActiveLocation attrs destroyed
      players@(p : _) -> do
        stillThere <- selectAny (EnemyWithId bellach)
        if stillThere
          then do
            immediateAttack p bellach
            doStep destroyed $ Run (WhenRevealed (toTarget attrs) : [EnemyAttack q bellach | q <- players])
          else when (destroyed > 0) $ placeDamageOnActiveLocation attrs destroyed
