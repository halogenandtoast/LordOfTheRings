module LordOfTheRings.Treachery.Cards.RogueWave (rogueWave) where

import LordOfTheRings.Card (CardType (QuestType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RogueWave = RogueWave TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rogueWave :: TreacheryCard RogueWave
rogueWave = treachery RogueWave Cards.rogueWave

-- | When Revealed: Each player must either exhaust a Ship objective he controls or
-- distribute 4 damage among characters he controls (6 damage instead if you are
-- off-course).
-- Shadow: Attacking enemy gets +1 attack for each quest card in the victory display.
instance RunMessage RogueWave where
  runMessage msg t@(RogueWave attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      off <- isOffCourse
      eachPlayer \p -> do
        ships <- select (shipObjective <> ReadyAlly <> AllyControlledBy (PlayerWithId p))
        chooseOneM p do
          questionLabeled "rogueWave.choose"
          causedBy attrs
          targets ships exhaust
          labeled "rogueWave.damage" $ dealDamageDivided attrs p (if off then 6 else 4)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      let quests = length $ filter ((== QuestType) . cdCardType . toCardDef) victory
      when (quests > 0) $ enemyGetsAttack (CardIdSource cid) eid quests
      pure t
    _ -> RogueWave <$> runMessage msg attrs
