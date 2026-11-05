module LordOfTheRings.Ally.Cards.TamedMumak (tamedMumak) where

import LordOfTheRings.Ally.Fleet (runShip, shipController)
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype TamedMumak = TamedMumak AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Objective-ally; each player takes control of one as stage 1A is set up.
tamedMumak :: AllyCard TamedMumak
tamedMumak = ally TamedMumak Cards.tamedMumak

-- | Immune to player card effects. Damage from undefended attacks against you must be
-- assigned to Tamed Mumak.
instance HasModifiersFor TamedMumak where
  getModifiersFor (TamedMumak a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    for_ (shipController a) \pid -> modified a (PlayerTarget pid) [UndefendedDamageTo (toTarget a)]

-- | If Tamed Mumak leaves play, the controlling player is eliminated. Like a Ship of the
-- Dream-chaser's Fleet, a discarded objective leaves the game.
instance RunMessage TamedMumak where
  runMessage msg (TamedMumak a) = case msg of
    Discard _ (isTarget a -> True) -> do
      eliminated
      TamedMumak <$> runShip (pure ()) msg a
    _ -> TamedMumak <$> runShip eliminated msg a
   where
    eliminated = traverse_ eliminatePlayer (shipController a)
