module LordOfTheRings.Treachery.Cards.SongOfSleep (songOfSleep) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SongOfSleep = SongOfSleep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfSleep :: TreacheryCard SongOfSleep
songOfSleep = treachery SongOfSleep Cards.songOfSleep

instance HasModifiersFor SongOfSleep where
  getModifiersFor (SongOfSleep a) = withHost a \host -> modified a host [Blank, CannotReady]

instance RunMessage SongOfSleep where
  runMessage msg t@(SongOfSleep a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      pid <- getRevealer
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
      free <- filterM (fmap null . treacheriesOn a.treacheryCardCode . toTarget) heroes
      chooseOrRunOneM pid do
        questionLabeled "oldForest.songOfSleep"
        causedBy a
        targets free \hero -> do
          removeFromQuest hero
          push $ WithTarget (toTarget hero) (DoStep 1 msg)
      pure t
    WithTarget host (DoStep 1 (WhenRevealed (isTarget a -> True))) -> do
      mods <- getModifiers host
      traits <- getTraits (toTarget a) (toCardDef a)
      let barred = CannotHaveAttachments `elem` mods
            || any (`notMember` traits) [trait | CannotHaveAttachmentsWithoutTrait trait <- mods]
      -- Removal from the quest still resolves if the hero cannot take a Condition.
      if barred then discard a a else attachTreachery a.id host
      pure t
    _ -> SongOfSleep <$> runMessage msg a
