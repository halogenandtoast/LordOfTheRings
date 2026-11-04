module LordOfTheRings.Enemy.Cards.Shelob (shelob) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (cleftInVictoryDisplay)

newtype Shelob = Shelob EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

shelob :: EnemyCard Shelob
shelob = enemy Shelob Cards.shelob

{- | Cannot have attachments. Shelob gets +1 threat for each resource on her.
While Cleft of Cirith Ungol is in the victory display, Shelob gets +2 attack, loses
Indestructible, and gains: "Shelob engages the first player." No other player can engage
her then.
-}
instance HasModifiersFor Shelob where
  getModifiersFor (Shelob a) = do
    cleft <- cleftInVictoryDisplay
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modifySelf a
      $ [CannotHaveAttachments, ThreatPerToken Resource]
      <> (if cleft then [AttackModifier 2, LoseKeyword Indestructible, EngagesOnly lead] <> map CannotEngagePlayer others else [])

-- | Forced: At the beginning of the quest phase, either place 1 resource on Shelob, or she
-- makes an immediate attack against the first player.
instance HasAbilities Shelob where
  getAbilities (Shelob a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

{- | The first player chooses. Once Cleft of Cirith Ungol is in the victory display, she
engages the first player: as it goes there, as she enters the staging area, and as the first
player token passes.
-}
instance RunMessage Shelob where
  runMessage msg e@(Shelob attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "shelob.choose"
        causedBy attrs
        labeled "shelob.resource" $ placeResources attrs attrs 1
        labeled "shelob.attack" $ immediateAttack lead attrs.id
      pure e
    CheckWindows ws | notNull [() | Window After (ExploredCard c) <- ws, toCardCode c == toCardCode Locations.cleftOfCirithUngol] -> do
      engageFirstPlayer
      Shelob <$> runMessage msg attrs
    SetFirstPlayer pid | attrs.placement /= EngagedWith pid -> do
      whenM cleftInVictoryDisplay $ engageEnemy pid attrs.id
      Shelob <$> runMessage msg attrs
    PlaceEnemy eid StagingArea | eid == attrs.id -> do
      cleft <- cleftInVictoryDisplay
      if cleft
        then do
          lead <- getLead
          engageEnemy lead attrs.id
          pure e
        else Shelob <$> runMessage msg attrs
    _ -> Shelob <$> runMessage msg attrs
   where
    engageFirstPlayer = do
      lead <- getLead
      when (attrs.placement /= EngagedWith lead) $ engageEnemy lead attrs.id
