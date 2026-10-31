-- | What the Revised Core campaign's cards share (Mirkwood Paths): Mendor, and boons and
-- burdens each player attaches to a hero.
module LordOfTheRings.Scenario.Scenarios.MirkwoodPaths (
  putMendorIntoPlay,
  mendorInPlay,
  eachPlayerAttaches,
  attachedInPlay,
  campaignPrisoner,
) where

import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Campaign (CampaignLogKey (Prisoner), recorded)
import LordOfTheRings.Card
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Id (HeroId)
import LordOfTheRings.Entities (entitiesAllies, entitiesAttachments, entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Scenario.Types (scenarioCampaign)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Setup: Put Mendor into play. The first player gains control of him.
putMendorIntoPlay :: ReverseQueue m => m ()
putMendorIntoPlay = do
  lead <- getLead
  putObjectiveAllyIntoPlayUnder lead =<< genEncounterCard Objectives.mendor

mendorInPlay :: HasGame m => m (Maybe Ally)
mendorInPlay = getsGame (find ((== toCardCode Objectives.mendor) . toCardCode) . toList . entitiesAllies . gameEntities)

-- | The cards of the code attached to heroes, with the hero each is on.
attachedInPlay :: HasGame m => CardDef -> m [(CardCode, HeroId)]
attachedInPlay def = getsGame \g ->
  let es = gameEntities g
   in [(toCardCode a, h) | a <- toList (entitiesAttachments es), toCardCode a == toCardCode def, AttachedTo (HeroTarget h) <- [a.placement]]
        <> [(toCardCode o, h) | o <- toList (entitiesObjectives es), toCardCode o == toCardCode def, AttachedTo (HeroTarget h) <- [o.placement]]

{- | Each player attaches 1 set-aside copy of the card to a hero they control, if able:
while copies are left, to a hero without one (Limit 1 per hero). A player card is the
player's own; an encounter card comes in as an objective attached to the hero.
-}
eachPlayerAttaches :: ReverseQueue m => CardDef -> Int -> m ()
eachPlayerAttaches def copies = do
  used <- length <$> attachedInPlay def
  players <- getRemainingPlayers
  for_ (zip players [1 .. copies - used]) \(pid, _) -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> not_ (HeroWithAttachment $ toCardCode def))
    chooseOrRunOneM pid do
      questionLabeled "campaign.attachTo"
      targets heroes \h ->
        if isPlayerCardDef def
          then (\card -> attachOutOfPlay pid card h) =<< genPlayerCard def (Just pid)
          else (`putObjectiveIntoPlayAttached` h) =<< genEncounterCard def

{- | Escape from Dol Guldur's campaign card: Forced: When a hero would be randomly selected
to be a "prisoner", the "prisoner" from the Campaign Log is selected instead, while that
hero is in play.
-}
campaignPrisoner :: HasGame m => m (Maybe HeroId)
campaignPrisoner = getsGame \g ->
  let named = foldMap (recorded Prisoner) (scenarioCampaign $ toAttrs $ gameScenario g)
   in headMay [h.id | h <- toList (entitiesHeroes $ gameEntities g), toCardCode h `elem` named]
