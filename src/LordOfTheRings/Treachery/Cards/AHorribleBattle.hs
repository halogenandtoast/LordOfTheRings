module LordOfTheRings.Treachery.Cards.AHorribleBattle (aHorribleBattle) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AHorribleBattle = AHorribleBattle TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aHorribleBattle :: TreacheryCard AHorribleBattle
aHorribleBattle = treachery AHorribleBattle Cards.aHorribleBattle

instance RunMessage AHorribleBattle where
  runMessage msg t@(AHorribleBattle a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        cards <- (.deck) <$> getPlayer pid
        able <- filterM (fmap (notElem CannotBePoisoned) . getModifiers) heroes
        chooseOrRunOneM pid do
          questionLabeled "aHorribleBattle.choose"
          causedBy a
          when (notNull heroes && length able == length heroes && length cards >= length heroes)
            $ labeled "aHorribleBattle.poison"
            $ traverse_ (\h -> givePoison h 1) heroes
          labeled "aHorribleBattle.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> AHorribleBattle <$> runMessage msg a
