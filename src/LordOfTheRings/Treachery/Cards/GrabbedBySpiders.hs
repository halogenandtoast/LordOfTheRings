module LordOfTheRings.Treachery.Cards.GrabbedBySpiders (grabbedBySpiders) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GrabbedBySpiders = GrabbedBySpiders TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge. Doomed 1.
grabbedBySpiders :: TreacheryCard GrabbedBySpiders
grabbedBySpiders = treachery GrabbedBySpiders Cards.grabbedBySpiders

-- | Counts as a Condition attachment with the text: "Forced: After the Search objective is
-- flipped to Off Track, each player discards an ally he controls. Then, discard Grabbed by
-- Spiders."
instance HasAbilities GrabbedBySpiders where
  getAbilities (GrabbedBySpiders a) =
    [mkAbility a 1 $ forced $ FlippedToWindow After (toCardCode Objectives.offTrack) | AttachedTo _ <- [a.placement]]

-- | When Revealed: Attach to the main quest. Limit 1 per quest: with a copy on it already,
-- this one is discarded unattached.
instance RunMessage GrabbedBySpiders where
  runMessage msg t@(GrabbedBySpiders attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      taken <- hostsOfTreachery (toCardCode Cards.grabbedBySpiders)
      mains <- getMainQuests
      for_ (take 1 $ filter (`notElem` taken) mains) $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        allies <- map toTarget <$> select (AllyControlledBy $ PlayerWithId p)
        unless (null allies) $ chooseAndDiscard attrs p allies
      discard attrs
      pure t
    _ -> GrabbedBySpiders <$> runMessage msg attrs
