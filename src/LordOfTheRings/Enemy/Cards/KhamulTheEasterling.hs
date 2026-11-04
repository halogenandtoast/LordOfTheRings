module LordOfTheRings.Enemy.Cards.KhamulTheEasterling (khamulTheEasterling) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

-- | Of the Power of Mordor deck (The Temple of Doom, The Fortress of Nurn).
newtype KhamulTheEasterling = KhamulTheEasterling EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

khamulTheEasterling :: EnemyCard KhamulTheEasterling
khamulTheEasterling = enemy KhamulTheEasterling Cards.khamulTheEasterling

-- | Immune to player card effects.
-- Khamûl the Easterling engages the first player: no other player can engage him.
-- Only the engaged player can declare attackers or defenders against Khamûl the
-- Easterling.
instance HasModifiersFor KhamulTheEasterling where
  getModifiersFor (KhamulTheEasterling a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modifySelf a $ [ImmuneToPlayerCardEffects, EngagesOnly lead, OnlyEngagedPlayerFights] <> map CannotEngagePlayer others

-- | As the first player token passes, he engages its new holder, as Chief Turch does;
-- anything else that would have another player engage him has the first player engage him.
instance RunMessage KhamulTheEasterling where
  runMessage msg (KhamulTheEasterling attrs) = case msg of
    SetFirstPlayer pid | EngagedWith p <- attrs.placement, p /= pid -> do
      engageEnemy pid attrs.id
      KhamulTheEasterling <$> runMessage msg attrs
    _ -> KhamulTheEasterling <$> runMessage msg attrs
