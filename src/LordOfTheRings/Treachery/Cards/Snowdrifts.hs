module LordOfTheRings.Treachery.Cards.Snowdrifts (snowdrifts) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Snowdrifts = Snowdrifts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowdrifts :: TreacheryCard Snowdrifts
snowdrifts = treachery Snowdrifts Cards.snowdrifts

-- | Counts as a Condition attachment with the text: "Limit 1 per location. No more than 3
-- progress can be placed on attached location each round."
instance HasModifiersFor Snowdrifts where
  getModifiersFor (Snowdrifts a) = case a.placement of
    AttachedTo (LocationTarget lid) -> modified a lid [ProgressPerRoundLimit 3]
    _ -> pure ()

-- | When Revealed: Attach to the active location, of two the one the first player chooses
-- (FAQ 1.34), that has no Snowdrifts. If Snowdrifts is not attached to the active location,
-- it gains surge.
instance RunMessage Snowdrifts where
  runMessage msg t@(Snowdrifts attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      actives <- select IsActiveLocation
      copies <- getsGame (filter ((== toCardCode attrs) . toCardCode) . toList . (.entities.treacheries))
      let free = [l | l <- actives, not (any ((== AttachedTo (toTarget l)) . (.placement)) copies)]
      lead <- getLead
      if null free
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "theRingGoesSouth.activeLocation"
          causedBy attrs
          targets free \l -> attachTreachery attrs.treacheryId l
      pure t
    _ -> Snowdrifts <$> runMessage msg attrs
