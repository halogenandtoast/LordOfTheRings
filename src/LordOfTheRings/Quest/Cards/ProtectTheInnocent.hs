module LordOfTheRings.Quest.Cards.ProtectTheInnocent (protectTheInnocent) where

import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ProtectTheInnocent = ProtectTheInnocent QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest of the Angmar Awakened campaign, which its campaign cards add to
-- the staging area.
protectTheInnocent :: QuestCard ProtectTheInnocent
protectTheInnocent = quest ProtectTheInnocent Cards.protectTheInnocent

{- | Forced: After an attack damages a character, place 1 damage on Protect the Innocent.
When progress would be placed on Protect the Innocent, remove that much damage from it
instead.
If Protect the Innocent has 10 or more damage on it (20 or more damage if there are 3 or 4
players in the game), the players lose the game.
-}
instance RunMessage ProtectTheInnocent where
  runMessage msg q@(ProtectTheInnocent attrs) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource _) target _) | isCharacter target -> do
      placeTokens attrs attrs Damage 1
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress n -> do
      removeTokens source attrs Damage n
      pure q
    PlaceTokens _ (isTarget attrs -> True) Damage n -> do
      players <- length <$> getPlayerOrder
      let limit = if players >= 3 then 20 else 10
      when (countTokens Damage attrs.questTokens + n >= limit) $ gameOver Lost
      ProtectTheInnocent <$> runMessage msg attrs
    _ -> ProtectTheInnocent <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
