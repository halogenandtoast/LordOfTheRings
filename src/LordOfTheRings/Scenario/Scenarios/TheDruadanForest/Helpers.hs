-- | What the cards of The Drúadan Forest share: the payments its Woses and treacheries ask
-- of the players' resource pools.
module LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (
  heroPaysOr,
  returnToStagingUnlessAnyPlayerPays,
) where

import LordOfTheRings.Classes.HasQueue (pushAll)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Resource))

{- | "The hero must pay 1 resource or ...": its controller pays 1 resource from its pool,
or the alternative happens; with nothing it can pay, the alternative happens.
-}
heroPaysOr
  :: (ReverseQueue m, Targetable source, Sourceable source)
  => source -> HeroId -> Text -> QueueT Message m () -> m ()
heroPaysOr source hid key orElse = do
  h <- getHero hid
  able <- heroCanSpend h
  if able && h.resources > 0
    then chooseOneM h.owner do
      questionLabeled "druadanForest.payOr"
      causedBy source
      aimedAt [hid]
      labeled "payResource" $ removeTokens source hid Resource 1
      labeled key orElse
    else pushAll =<< capture orElse

{- | "Return attacking enemy to the staging area after it attacks unless any player pays 1
resource": once the attack resolves, each player in turn may pay, and the enemy returns
unless one does.
-}
returnToStagingUnlessAnyPlayerPays :: Targetable source => source -> EnemyId -> QueueT Message GameT ()
returnToStagingUnlessAnyPlayerPays source eid = do
  players <- getPlayerOrder
  afterAttack eid =<< capture (offer players)
 where
  offer :: ReverseQueue n => [PlayerId] -> n ()
  offer = \case
    [] -> placeEnemy eid StagingArea
    p : rest -> do
      able <- (> 0) <$> getResourcesOfSphere p Neutral
      if able
        then chooseOneM p do
          questionLabeled "druadanForest.payToKeepEngaged"
          causedBy source
          aimedAt [eid]
          labeled "payResource" $ spendResources p Neutral 1
          labeled "druadanForest.declineToPay" $ offer rest
        else offer rest
