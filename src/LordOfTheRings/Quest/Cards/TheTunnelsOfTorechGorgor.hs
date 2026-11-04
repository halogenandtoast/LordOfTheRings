module LordOfTheRings.Quest.Cards.TheTunnelsOfTorechGorgor (theTunnelsOfTorechGorgor) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheTunnelsOfTorechGorgor = TheTunnelsOfTorechGorgor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
theTunnelsOfTorechGorgor :: QuestCard TheTunnelsOfTorechGorgor
theTunnelsOfTorechGorgor = quest TheTunnelsOfTorechGorgor Cards.theTunnelsOfTorechGorgor

-- | During the travel phase, the players must travel to a location, if able: each location
-- in the staging area must be traveled to, of those the players can travel to.
-- Progress cannot be placed on this quest unless there are at least 3 resources here.
instance HasModifiersFor TheTunnelsOfTorechGorgor where
  getModifiersFor (TheTunnelsOfTorechGorgor a) = do
    locations <- select LocationInStagingArea
    for_ locations \l -> modified a l [MustBeTraveledTo]
    when (countTokens Resource a.questTokens < 3) $ modifySelf a [ProgressLimit 0]

-- | Forced: When the active location is explored, place 1 resource here.
instance HasAbilities TheTunnelsOfTorechGorgor where
  getAbilities (TheTunnelsOfTorechGorgor a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow When]

instance RunMessage TheTunnelsOfTorechGorgor where
  runMessage msg q@(TheTunnelsOfTorechGorgor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure q
    _ -> TheTunnelsOfTorechGorgor <$> runMessage msg attrs
