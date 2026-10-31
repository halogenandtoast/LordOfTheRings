module LordOfTheRings.Enemy.Cards.DunlandAssassin (dunlandAssassin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DunlandAssassin = DunlandAssassin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandAssassin :: EnemyCard DunlandAssassin
dunlandAssassin = enemy DunlandAssassin Cards.dunlandAssassin

-- | Dunland Assassin cannot be optionally engaged by a player with 3 or more cards in his
-- hand.
instance HasModifiersFor DunlandAssassin where
  getModifiersFor (DunlandAssassin a) = modified a a [CannotBeOptionallyEngagedBy (PlayerWithCardsInHand 3)]

-- | While Dunland Assassin is in the staging area, it gains: "Forced: After the first
-- player draws any number of cards, Dunland Assassin makes an immediate attack against
-- him." Answered as the draw's window opens, as Mugash's Guard answers its windows.
instance RunMessage DunlandAssassin where
  runMessage msg (DunlandAssassin attrs) = case msg of
    CheckWindows ws | attrs.placement == StagingArea -> do
      lead <- getLead
      when (lead `elem` [pid | Window After (CardsDrawn pid _) <- ws]) $ immediateAttack lead attrs.id
      DunlandAssassin <$> runMessage msg attrs
    _ -> DunlandAssassin <$> runMessage msg attrs
