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

Its player resolves it alone (Peril), so he chooses the hero, among the heroes committed to
the quest by any player. With none committed there is no hero to remove or to attach to,
and like any treachery that does not say otherwise it goes to the discard pile once it has
resolved (Rules Reference, "Staging Area"). The attached hero is read off its windows for
committing and being declared, and its abilities as they are used.
-}
instance RunMessage SarumansVoice where
  runMessage msg t@(SarumansVoice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      committed <- select CommittedHero
      chooseOrRunOneM you do
        questionLabeled "sarumansVoice.attach"
        causedBy attrs
        targets committed \h -> do
          removeFromQuest h
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
