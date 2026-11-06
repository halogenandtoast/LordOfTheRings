module LordOfTheRings.Quest.Cards.ThroughTheRuinsOsgiliath (throughTheRuinsOsgiliath) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (WestBank))

newtype ThroughTheRuinsOsgiliath = ThroughTheRuinsOsgiliath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheRuinsOsgiliath :: QuestCard ThroughTheRuinsOsgiliath
throughTheRuinsOsgiliath = quest ThroughTheRuinsOsgiliath Cards.throughTheRuinsOsgiliath

instance HasModifiersFor ThroughTheRuinsOsgiliath where
  getModifiersFor (ThroughTheRuinsOsgiliath a) = when a.flipped do
    west <- select (LocationWithTrait WestBank)
    for_ west \lid -> modified a lid [CannotBeTraveledTo]
    players <- getPlayerOrder
    for_ players \pid -> modified a (PlayerTarget pid) [CardsFromHandPerRoundLimit 1]
