module LordOfTheRings.Treachery.Cards.EyesInTheDark (eyesInTheDark) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EyesInTheDark = EyesInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eyesInTheDark :: TreacheryCard EyesInTheDark
eyesInTheDark = treachery EyesInTheDark Cards.eyesInTheDark

-- | Doomed 1.
-- When Revealed: Each player must choose: either raise your threat by 1 for each questing
-- character you control, or discard a questing character you control.
-- Shadow: If this attack is undefended, discard an ally you control.
instance RunMessage EyesInTheDark where
  runMessage msg t@(EyesInTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        questing <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId p)) (CommittedAlly <> AllyControlledBy (PlayerWithId p))
        chooseOneM p do
          questionLabeled "eyesInTheDark.choose"
          causedBy attrs
          labeledWith "eyesInTheDark.raiseThreat" [intVar "n" (length questing)]
            $ encounterRaisesThreat p (length questing)
          unless (null questing) $ labeled "eyesInTheDark.discard" do
            chooseOrRunOneM p do
              questionLabeled "eyesInTheDark.character"
              causedBy attrs
              targets questing (discard attrs)
      pure t
    ResolveShadowEffect cid _ pid [] | cid == attrs.cardId -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "eyesInTheDark.discardAlly"
        causedBy attrs
        targets allies (discard attrs)
      pure t
    _ -> EyesInTheDark <$> runMessage msg attrs
