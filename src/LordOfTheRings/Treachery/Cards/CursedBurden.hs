module LordOfTheRings.Treachery.Cards.CursedBurden (cursedBurden) where

import LordOfTheRings.Card (toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (controlledKeys, sharingClanTrait)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CursedBurden = CursedBurden TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedBurden :: TreacheryCard CursedBurden
cursedBurden = treachery CursedBurden Cards.cursedBurden

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Attached hero cannot
-- commit to the quest, and cannot attack or defend against enemies with a Trait shared by
-- any of that hero's attached Key objectives."
instance HasModifiersFor CursedBurden where
  getModifiersFor (CursedBurden a) = case a.placement of
    AttachedTo host -> do
      heldKeys <- map toCardDef . filter ((== AttachedTo host) . (.placement)) <$> controlledKeys
      modified a host [CannotCommitToQuest, CannotFight (sharingClanTrait heldKeys)]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to a hero with a Key objective attached, if able.
instance RunMessage CursedBurden where
  runMessage msg t@(CursedBurden attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      keyed <- ordNub . concatMap (\o -> [h | AttachedTo h <- [o.placement]]) <$> controlledKeys
      burdened <- getsGame (map (.placement) . filter ((== toCardCode attrs) . toCardCode) . toList . (.entities.treacheries))
      let hosts = filter ((`notElem` burdened) . AttachedTo) keyed
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "cursedBurden.attach"
        causedBy attrs
        targets hosts (attachTreachery attrs.treacheryId)
      pure t
    _ -> CursedBurden <$> runMessage msg attrs
