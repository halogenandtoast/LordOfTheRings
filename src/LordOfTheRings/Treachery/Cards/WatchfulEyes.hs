module LordOfTheRings.Treachery.Cards.WatchfulEyes (watchfulEyes) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WatchfulEyes = WatchfulEyes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

watchfulEyes :: TreacheryCard WatchfulEyes
watchfulEyes = treachery WatchfulEyes Cards.watchfulEyes

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Forced: If
-- attached hero is exhausted at the end of the combat phase, reveal 1 card from the
-- encounter deck and add it to the staging area."
instance HasAbilities WatchfulEyes where
  getAbilities (WatchfulEyes a) = case a.placement of
    AttachedTo (HeroTarget hid) ->
      [restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero) $ forced $ PhaseEndsWindow When CombatPhase]
    _ -> []

-- | When Revealed: The first player attaches Watchful Eyes to one of his heroes.
instance RunMessage WatchfulEyes where
  runMessage msg t@(WatchfulEyes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      -- limit 1 per hero
      heroes <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) =<< select (HeroControlledBy $ PlayerWithId lead)
      chooseOrRunOneM lead do
        questionLabeled "watchfulEyes.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure t
    _ -> WatchfulEyes <$> runMessage msg attrs
