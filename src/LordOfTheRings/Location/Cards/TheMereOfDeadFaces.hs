module LordOfTheRings.Location.Cards.TheMereOfDeadFaces (theMereOfDeadFaces) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype TheMereOfDeadFaces = TheMereOfDeadFaces LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMereOfDeadFaces :: LocationCard TheMereOfDeadFaces
theMereOfDeadFaces = location TheMereOfDeadFaces Cards.theMereOfDeadFaces

-- | X is the number of resource tokens here. Cannot leave the staging area.
-- If Gollum is not in play, any resource tokens that would be placed on Gollum are placed
-- here instead. (The Dead Marshes' 'placeOnGollum' places them.)
instance HasModifiersFor TheMereOfDeadFaces where
  getModifiersFor (TheMereOfDeadFaces a) = do
    let n = countTokens Resource a.locationTokens
    modifySelf a $ CannotLeaveStagingArea : [ThreatModifier n | n > 0]

-- | Forced: After Gollum enters play, place 2 resource tokens on Gollum, then move all
-- resource tokens from The Mere of Dead Faces to Gollum.
instance RunMessage TheMereOfDeadFaces where
  runMessage msg (TheMereOfDeadFaces a) = case msg of
    CheckWindows ws | gollum : _ <- [t | Window After (EntersStagingArea t@(ObjectiveTarget _)) <- ws] -> do
      isGollum <- case gollum of
        ObjectiveTarget oid -> (== toCardCode Objectives.gollumTheDeadMarshes) . toCardCode <$> getObjective oid
        _ -> pure False
      when isGollum do
        let n = countTokens Resource a.locationTokens
        placeResources a gollum (2 + n)
        when (n > 0) $ removeTokens a a Resource n
      TheMereOfDeadFaces <$> runMessage msg a
    _ -> TheMereOfDeadFaces <$> runMessage msg a
