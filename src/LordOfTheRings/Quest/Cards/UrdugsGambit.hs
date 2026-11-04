module LordOfTheRings.Quest.Cards.UrdugsGambit (urdugsGambit) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getCommittedWillpower)
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints, getStagingThreat)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo, Unplaced))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype UrdugsGambit = UrdugsGambit QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugsGambit :: QuestCard UrdugsGambit
urdugsGambit = quest UrdugsGambit Cards.urdugsGambit

-- | Progress cannot be placed on this quest from questing successfully.
-- When the players quest unsuccessfully, they do not raise their threat.
-- The stage is not defeated by its progress: the players win as Urdug runs out of hit
-- points, and lose as the progress reaches its quest points.
instance HasModifiersFor UrdugsGambit where
  getModifiersFor (UrdugsGambit a) = when a.flipped do
    modified a GameTarget [QuestProgressCanceledBy (toSource a), NoThreatFromQuestingUnsuccessfully]
    modifySelf a [CannotBeDefeated]

-- | Forced: When the players quest unsuccessfully, they do not raise their threat. Instead,
-- place X progress here, where X is the amount by which they would have raised their
-- threat. Then, if this stage has progress equal to its quest points, the players lose the
-- game.
instance HasAbilities UrdugsGambit where
  getAbilities (UrdugsGambit a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow When | a.flipped]

urdug :: EnemyMatcher
urdug = EnemyWithCardCode (toCardCode Enemies.urdugEnemy)

{- | When Revealed: The first player loses control of both Urdug and Tiny, flips them to
their enemy sides, and adds them to the staging area. (Remove any damage that was on them.)
Each other player reveals an encounter card.
Urdug keeps his horn as he flips (Vision of the Palantir's Roam Across Rhovanion review):
it is taken off him before he turns over, which discards what is on him, and put back on
his enemy side.
When Urdug has no hit points remaining, the players win the game.
-}
instance RunMessage UrdugsGambit where
  runMessage msg q@(UrdugsGambit attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      UrdugsGambit <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- getsGame (toList . (.entities.allies))
      horns <- getsGame (filter ((== toCardCode Objectives.urdugsHorn) . toCardCode) . toList . (.entities.objectives))
      for_ (filter ((== toCardCode Objectives.urdug) . toCardCode) allies) \u -> do
        let worn = [h | h <- horns, h.placement == AttachedTo (toTarget u)]
        for_ worn \h -> placeObjective h.id Unplaced
        flipOver (toId u)
        for_ worn \h -> placeObjective h.id (AttachedTo $ EnemyTarget $ EnemyId $ unAllyId $ toId u)
      for_ (filter ((== toCardCode Objectives.tiny) . toCardCode) allies) $ flipOver . toId
      others <- drop 1 <$> getRemainingPlayers
      revealEncounterCards (length others)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- (-) <$> getStagingThreat <*> getCommittedWillpower
      when (x > 0) $ placeProgress attrs attrs x
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      questPoints <- getQuestPoints attrs
      when (attrs.progress >= questPoints) $ gameOver Lost
      pure q
    -- damage dealt to Urdug may spend his last hit point; he is Indestructible
    CheckDefeated _ (EnemyTarget eid) | attrs.flipped -> do
      isUrdug <- selectAny (EnemyWithId eid <> urdug)
      when isUrdug do
        spent <- (<=) <$> getEnemyHitPoints eid <*> getDamage (toTarget eid)
        when spent winGame
      UrdugsGambit <$> runMessage msg attrs
    _ -> UrdugsGambit <$> runMessage msg attrs
