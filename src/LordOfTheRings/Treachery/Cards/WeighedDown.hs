module LordOfTheRings.Treachery.Cards.WeighedDown (weighedDown) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeighedDown = WeighedDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weighedDown :: TreacheryCard WeighedDown
weighedDown = treachery WeighedDown Cards.weighedDown

-- | Counts as a Condition attachment with the text: "Forced: After attached hero readies,
-- choose and discard an attachment from it. You cannot choose Weighed Down unless attached
-- hero has no other non-objective attachments."
instance HasAbilities WeighedDown where
  getAbilities (WeighedDown a) = case a.placement of
    AttachedTo host@(HeroTarget _) -> [mkAbility a 1 $ forced $ BecameReadyWindow After host]
    _ -> []

-- | When Revealed: Attach to the hero with the most attachments without Weighed Down
-- attached, the first player choosing among those tied (FAQ 1.03). Then, exhaust attached
-- hero.
instance RunMessage WeighedDown where
  runMessage msg t@(WeighedDown attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- map toTarget <$> select AnyHero
      counts <- for heroes \h -> do
        weighed <- treacheriesOn attrs.treacheryCardCode h
        n <- length <$> getAttachmentsOn h
        pure [(h, n) | null weighed]
      let eligible = concat counts
          most = foldr (max . snd) 0 eligible
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "weighedDown.attach"
        causedBy attrs
        targets [h | (h, n) <- eligible, n == most] \h -> do
          attachTreachery attrs.treacheryId h
          exhaust h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> do
        attached <- getAttachmentsOn host
        let others = filter (/= toTarget attrs) attached
            choices = if all isObjective others then attached else others
        controller <- characterController host
        for_ controller \p ->
          chooseOrRunOneM p do
            questionLabeled "weighedDown.discard"
            causedBy attrs
            targets choices discard
      pure t
    _ -> WeighedDown <$> runMessage msg attrs
   where
    isObjective = \case
      ObjectiveTarget _ -> True
      _ -> False
