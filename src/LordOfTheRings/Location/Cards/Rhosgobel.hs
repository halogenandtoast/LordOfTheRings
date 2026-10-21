module LordOfTheRings.Location.Cards.Rhosgobel (rhosgobel) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype Rhosgobel = Rhosgobel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

rhosgobel :: LocationCard Rhosgobel
rhosgobel = location Rhosgobel Cards.rhosgobel

-- | X is the number of players in the game.
-- Travel: Players must complete stage one of this quest before they can travel to
-- Rhosgobel. (While it is in the staging area Wilyador cannot be healed: see Wilyador.)
instance HasModifiersFor Rhosgobel where
  getModifiersFor (Rhosgobel a) = do
    players <- length <$> getRemainingPlayers
    stageOne <- getsGame (any ((== toCardCode Quests.theWoundedEagle) . toCardCode) . toList . (.entities.quests))
    modified a a $ ThreatModifier players : [CannotBeTraveledTo | stageOne]
