module LordOfTheRings.Treachery.Cards.GallopingBoulders (gallopingBoulders) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (stoneGiant)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GallopingBoulders = GallopingBoulders TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gallopingBoulders :: TreacheryCard GallopingBoulders
gallopingBoulders = treachery GallopingBoulders Cards.gallopingBoulders

{- | When Revealed: The first player chooses a questing character. That character takes 3
damage and is removed from the quest.
Shadow: Put this card on top of the encounter deck.

Stone-giant: "While at least one Stone-giant is in the staging area, the Galloping
Boulders card gains surge." Asked once the card has been revealed, whether or not its
When Revealed effect was canceled, as a keyword it has.
-}
instance RunMessage GallopingBoulders where
  runMessage msg t@(GallopingBoulders attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questing <- getCommittedCharacters
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "gallopingBoulders.choose"
        causedBy attrs
        targets questing \c -> do
          dealDamage attrs c 3
          removeFromQuest c
      pure t
    CheckWindows ws | Window After (Revealed $ toTarget attrs) `elem` ws -> do
      whenM (selectAny $ stoneGiant <> EnemyInStagingArea) $ revealEncounterCardAfter (toTarget attrs)
      GallopingBoulders <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      removeShadowCard eid cid
      putOnTopOfEncounterDeck (MkEncounterCard cid (toCardCode attrs))
      pure t
    _ -> GallopingBoulders <$> runMessage msg attrs
