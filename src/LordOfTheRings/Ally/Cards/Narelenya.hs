module LordOfTheRings.Ally.Cards.Narelenya (narelenya, narelenyaUpgraded) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Phase (Phase (ResourcePhase))

newtype Narelenya = Narelenya AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

narelenya :: AllyCard Narelenya
narelenya = ally Narelenya Cards.narelenya

-- | Its Upgraded side (the Dream-chaser campaign): the first card, not only ally.
narelenyaUpgraded :: AllyCard Narelenya
narelenyaUpgraded = ally Narelenya Cards.narelenyaUpgraded

-- | Immune to player card effects.
instance HasModifiersFor Narelenya where
  getModifiersFor (Narelenya a) = shipModifiers a

-- | Reduce the cost of the first ally you play each round by 1: the reduction waits for
-- it from the start of each round.
instance HasAbilities Narelenya where
  getAbilities (Narelenya a) = [mkAbility a 1 $ silent $ PhaseBeginsWindow After ResourcePhase]

-- | If Nárelenya is destroyed, its controller is eliminated from the game.
instance RunMessage Narelenya where
  runMessage msg s@(Narelenya a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ (shipController a) \pid -> roundModifier a (PlayerTarget pid) (ReduceNextCostOf (if toCardCode a == toCardCode Cards.narelenyaUpgraded then AnyCard else CardWithType AllyType) 1)
      pure s
    _ -> Narelenya <$> runShip (traverse_ eliminatePlayer (shipController a)) msg a
