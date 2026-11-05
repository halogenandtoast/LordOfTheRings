module LordOfTheRings.Treachery.Cards.ImpenetrableDark (impenetrableDark) where

import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ImpenetrableDark = ImpenetrableDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

impenetrableDark :: TreacheryCard ImpenetrableDark
impenetrableDark = treachery ImpenetrableDark Cards.impenetrableDark

instance RunMessage ImpenetrableDark where
  runMessage msg t@(ImpenetrableDark a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        questing <- filterM (fmap not . immuneToSource (toSource a)) =<< selectCharacters (HeroControlledBy (PlayerWithId pid) <> CommittedHero) (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
        chooseOrRunOneM pid do
          questionLabeled "impenetrableDark.remove"
          causedBy a
          targets questing \c -> do
            removeFromQuest c
            exhaust c
      roundModifier a GameTarget CannotBeReadiedByPlayerCards
      pure t
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      available <- filterM (fmap not . immuneToSource (toSource a)) =<< exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "impenetrableDark.exhaust"
        causedBy a
        targets available \c -> do
          exhaust c
          roundModifier a c CannotReady
      pure t
    _ -> ImpenetrableDark <$> runMessage msg a
