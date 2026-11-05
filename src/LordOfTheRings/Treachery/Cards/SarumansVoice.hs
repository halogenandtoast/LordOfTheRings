module LordOfTheRings.Treachery.Cards.SarumansVoice (sarumansVoice) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SarumansVoice = SarumansVoice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden (The Road to Isengard). Peril. Doomed 2, as its card def says.
sarumansVoice :: TreacheryCard SarumansVoice
sarumansVoice = treachery SarumansVoice Cards.sarumansVoice

{- | When Revealed: Remove a hero from the quest and attach Saruman's Voice to that hero.
(Counts as a Condition attachment with the text: "Forced: When attached hero commits to
the quest, is declared as an attacker or defender, or triggers its ability, its controller
must discard 1 card from his hand.")

Its player resolves it alone (Peril), so he chooses the hero: one committed to the quest,
or any hero when none is, as it is revealed outside the quest phase. The attached hero is
read off its windows for committing and being declared, and its abilities as they are
used.
-}
instance RunMessage SarumansVoice where
  runMessage msg t@(SarumansVoice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      committed <- select CommittedHero
      heroes <- if null committed then select AnyHero else pure committed
      chooseOrRunOneM you do
        questionLabeled "sarumansVoice.attach"
        causedBy attrs
        targets heroes \h -> do
          when (h `elem` committed) $ removeFromQuest h
          attachTreachery attrs.treacheryId h
      pure t
    CheckWindows ws | AttachedTo host <- attrs.placement -> do
      let triggered =
            [() | Window After (CommittedToQuest c) <- ws, c == host]
              <> [() | Window After (DeclaredAsAttacker c _) <- ws, c == host]
              <> [() | Window After (DeclaredAsDefender c _) <- ws, c == host]
      voice host (length triggered)
      SarumansVoice <$> runMessage msg attrs
    UseAbility _ ab _ | AttachedTo host <- attrs.placement, sourceTarget ab.abilitySource == Just host -> do
      voice host 1
      SarumansVoice <$> runMessage msg attrs
    _ -> SarumansVoice <$> runMessage msg attrs
   where
    voice host n = when (n > 0) $ characterController host >>= traverse_ \pid -> chooseAndDiscardFromHand pid attrs n
