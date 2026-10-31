module LordOfTheRings.Treachery.Cards.CorruptedFlora (corruptedFlora) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CorruptedFlora = CorruptedFlora TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corruptedFlora :: TreacheryCard CorruptedFlora
corruptedFlora = treachery CorruptedFlora Cards.corruptedFlora

committedOf :: HasGame m => PlayerId -> m [Target]
committedOf p = selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId p)) (CommittedAlly <> AllyControlledBy (PlayerWithId p))

{- | When Revealed: Each player discards the bottom card of his deck. Each player chooses and
removes his characters from the quest until only X or fewer characters are questing, where
X is the printed cost of the card that player discarded. Deal 1 damage to each character
still committed to the quest.
Each player removes his characters one at a time, as the steps counting down for him.
-}
instance RunMessage CorruptedFlora where
  runMessage msg t@(CorruptedFlora attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        bottom <- discardBottomOfDeck p
        committed <- committedOf p
        doStep (length committed - maybe 0 printedCostOf bottom) (UseCardAbility p (toSource attrs) 1 [])
      pure t
    DoStep k inner@(UseCardAbility p (isSource attrs -> True) 1 _) -> do
      committed <- committedOf p
      if k > 0
        then do
          chooseOrRunOneM p do
            questionLabeled "corruptedFlora.remove"
            causedBy attrs
            targets committed removeFromQuest
          doStep (k - 1) inner
        else for_ committed \c -> dealDamage attrs c 1
      pure t
    _ -> CorruptedFlora <$> runMessage msg attrs
