module LordOfTheRings.Treachery.Cards.GatheringGround (gatheringGround) where

import LordOfTheRings.Helpers.Staging (getLocationQuestPoints, getLocationThreat)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GatheringGround = GatheringGround TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gatheringGround :: TreacheryCard GatheringGround
gatheringGround = treachery GatheringGround Cards.gatheringGround

-- | Counts as a Condition attachment with the text: "Each enemy revealed from the
-- encounter deck gains surge."
instance HasAbilities GatheringGround where
  getAbilities (GatheringGround a) = case a.placement of
    AttachedTo (LocationTarget _) -> [mkAbility a 1 $ forced $ EnemyRevealedWindow After]
    _ -> []

-- | When Revealed: Attach this card to a location in the staging area with the highest
-- combined threat and remaining quest points.
instance RunMessage GatheringGround where
  runMessage msg t@(GatheringGround attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- select LocationInStagingArea
      scored <- for locations \l -> do
        threat <- getLocationThreat l
        questPoints <- getLocationQuestPoints l
        progress <- (.progress) <$> getLocation l
        pure (l, threat + questPoints - progress)
      let best = foldr (max . snd) 0 scored
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "gatheringGround.attach"
        causedBy attrs
        targets [l | (l, s) <- scored, s == best] $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure t
    _ -> GatheringGround <$> runMessage msg attrs
