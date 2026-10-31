module LordOfTheRings.Quest.Cards.RaidGolfimbulsVault (raidGolfimbulsVault) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RaidGolfimbulsVault = RaidGolfimbulsVault QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge. Capture 3.
-- Forced: After a card is captured by another card effect, capture a random card from each
-- player's hand underneath Raid Golfimbul's Vault.
-- Forced: When the captured cards underneath Raid Golfimbul's Vault are rescued, put each
-- rescued Item card into its owner's hand and discard the rest.
--
-- The scenario keeps the captured cards, and resolves both.
raidGolfimbulsVault :: QuestCard RaidGolfimbulsVault
raidGolfimbulsVault = quest RaidGolfimbulsVault Cards.raidGolfimbulsVault

instance RunMessage RaidGolfimbulsVault where
  runMessage msg (RaidGolfimbulsVault attrs) = case msg of
    -- the scenario keeps the cards captured underneath it
    PlaceUnderneath (isTarget attrs -> True) _ -> pure $ RaidGolfimbulsVault attrs
    _ -> RaidGolfimbulsVault <$> runMessage msg attrs
