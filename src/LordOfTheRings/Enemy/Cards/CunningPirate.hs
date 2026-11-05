module LordOfTheRings.Enemy.Cards.CunningPirate (cunningPirate) where

import LordOfTheRings.Card (cdCost, costAsResources, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype CunningPirate = CunningPirate EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

cunningPirate :: EnemyCard CunningPirate
cunningPirate = enemy CunningPirate Cards.cunningPirate

-- | Cunning Pirate gets +1 attack and +1 defense for each resource on it.
instance HasModifiersFor CunningPirate where
  getModifiersFor (CunningPirate a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modified a a [AttackModifier n, DefenseModifier n]

-- | Forced: When Cunning Pirate engages you, discard an attachment you control and place
-- resources on Cunning Pirate equal to that attachment's printed cost.
instance HasAbilities CunningPirate where
  getAbilities (CunningPirate a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage CunningPirate where
  runMessage msg e@(CunningPirate attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      attachments <- getsGame (toList . (.entities.attachments))
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "cunningPirate.discard"
          causedBy attrs
          targets [x | x <- attachments, (toAttrs x).controller == pid, AttachedTo _ <- [x.placement]] \x -> do
            discard attrs x
            placeResources attrs attrs $ maybe 0 costAsResources (cdCost $ toCardDef x)
      pure e
    _ -> CunningPirate <$> runMessage msg attrs
