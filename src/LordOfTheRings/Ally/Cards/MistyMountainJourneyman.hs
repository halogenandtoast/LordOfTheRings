module LordOfTheRings.Ally.Cards.MistyMountainJourneyman (mistyMountainJourneyman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype MistyMountainJourneyman = MistyMountainJourneyman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mistyMountainJourneyman :: AllyCard MistyMountainJourneyman
mistyMountainJourneyman = ally MistyMountainJourneyman Cards.mistyMountainJourneyman

-- | You cannot play Misty Mountain Journeyman from your hand unless you are engaged with an
-- enemy.
instance HasModifiersFor MistyMountainJourneyman where
  getModifiersFor (MistyMountainJourneyman a) = when (a.placement == Unplaced) do
    engaged <- selectAny $ EnemyEngagedWith (PlayerWithId a.owner)
    unless engaged $ modified a (CardIdTarget a.allyCardId) [CannotBePlayedFromHand]
