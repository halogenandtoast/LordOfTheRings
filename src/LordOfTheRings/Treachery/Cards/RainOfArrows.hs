module LordOfTheRings.Treachery.Cards.RainOfArrows (rainOfArrows) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype RainOfArrows = RainOfArrows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rainOfArrows :: TreacheryCard RainOfArrows
rainOfArrows = treachery RainOfArrows Cards.rainOfArrows

instance RunMessage RainOfArrows where
  runMessage msg t@(RainOfArrows a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      objectives <- getsGame (map toId . filter ((== toCardCode Objectives.theDefenseOfHelmsDeep) . toCardCode) . toList . (.entities.objectives))
      available <- filterM (fmap ((> 0) . countTokens Resource . objectiveTokens . toAttrs) . getObjective) objectives
      locations <- select LocationInStagingArea
      chooseOrRunOneM lead do
        questionLabeled "rainOfArrows.choose"
        causedBy a
        when (notNull available) $ labeled "rainOfArrows.resource" do
          for_ available \oid -> removeTokens a oid Resource 1
        labeled "rainOfArrows.archery" do
          for_ locations \lid -> roundModifier a lid (GainKeyword $ Archery 1)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> RainOfArrows <$> runMessage msg a
