module LordOfTheRings.Treachery.Cards.ShelobsPoison (shelobsPoison) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (shelob)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShelobsPoison = ShelobsPoison TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shelobsPoison :: TreacheryCard ShelobsPoison
shelobsPoison = treachery ShelobsPoison Cards.shelobsPoison

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Treat attached
-- hero's text box as blank, except for Traits. Attached hero cannot ready."
instance HasModifiersFor ShelobsPoison where
  getModifiersFor (ShelobsPoison a) = withHost a \host -> modified a host [Blank, CannotReady]

{- | When Revealed: Attach to a hero you control and exhaust that hero. The player chooses
among his heroes without a Shelob's Poison.
Shadow: If attacking enemy is Shelob, resolve the "When Revealed" effect on Shelob's
Poison: the defending player attaches it.
-}
instance RunMessage ShelobsPoison where
  runMessage msg t@(ShelobsPoison attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- poisonableHeroes you
      chooseOrRunOneM you do
        questionLabeled "shelobsPoison.attach"
        causedBy attrs
        targets heroes \hid -> do
          attachTreachery attrs.treacheryId hid
          exhaust hid
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      isShelob <- selectAny (EnemyWithId eid <> shelob)
      when isShelob do
        heroes <- poisonableHeroes pid
        chooseOrRunOneM pid do
          questionLabeled "shelobsPoison.attach"
          causedBy attrs
          targets heroes \hid -> do
            push $ PutShadowTreacheryIntoPlay eid cid (AttachedTo $ HeroTarget hid)
            exhaust hid
      pure t
    _ -> ShelobsPoison <$> runMessage msg attrs
   where
    poisonableHeroes pid = do
      taken <- hostsOfTreachery (toCardCode Cards.shelobsPoison)
      filter (\h -> not $ any (isTarget h) taken) <$> select (HeroControlledBy $ PlayerWithId pid)
