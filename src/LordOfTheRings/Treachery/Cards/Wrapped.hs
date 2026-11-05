module LordOfTheRings.Treachery.Cards.Wrapped (wrapped) where

import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Tentacle))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Wrapped = Wrapped TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

wrapped :: TreacheryCard Wrapped
wrapped = treachery Wrapped Cards.wrapped

-- | Counts as a Tentacle attachment with the text: "Limit 1 per hero. Attached hero cannot
-- exhaust or ready. At the end of the round, discard attached hero from play. Combat
-- Action: Exhaust a hero you control without a Tentacle attachment to discard Wrapped!."
instance HasModifiersFor Wrapped where
  getModifiersFor (Wrapped a) = withHost a \host -> modified a host [CannotExhaust, CannotReady]

instance HasAbilities Wrapped where
  getAbilities (Wrapped a) = case a.placement of
    AttachedTo _ ->
      [ mkAbility a 1 $ forced RoundEndsWindow
      , restricted a 2 (DuringPhase CombatPhase)
          $ ActionAbility
          $ ExhaustHeroCost (HeroControlledBy ActivePlayer <> not_ (HeroWithAttached Tentacle))
      ]
    _ -> []

-- | When Revealed: The first player attaches Wrapped! to a hero he controls.
instance RunMessage Wrapped where
  runMessage msg t@(Wrapped attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      -- limit 1 per hero
      heroes <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) =<< select (HeroControlledBy $ PlayerWithId lead)
      chooseOrRunOneM lead do
        questionLabeled "wrapped.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs (discard attrs)
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      discard attrs attrs
      pure t
    _ -> Wrapped <$> runMessage msg attrs
