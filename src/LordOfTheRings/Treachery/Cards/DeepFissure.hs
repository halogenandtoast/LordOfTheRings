module LordOfTheRings.Treachery.Cards.DeepFissure (deepFissure) where

import LordOfTheRings.Card (cdThreat, cdWillpower, toCardDef)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeepFissure = DeepFissure TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepFissure :: TreacheryCard DeepFissure
deepFissure = treachery DeepFissure Cards.deepFissure

{- | Peril. When Revealed: Remove a character you control from the quest and discard the
top card of the encounter deck. If that character's printed willpower is less than the
discarded card's threat, discard that character. With no character of yours committed,
the card is still discarded.
-}
instance RunMessage DeepFissure where
  runMessage msg t@(DeepFissure attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      committed <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId you)) (CommittedAlly <> AllyControlledBy (PlayerWithId you))
      if null committed
        then discardTopOfEncounterDeckFor attrs (PlayerTarget you) 1
        else chooseOneM you do
          questionLabeled "deepFissure.remove"
          causedBy attrs
          targets committed \c -> do
            removeFromQuest c
            discardTopOfEncounterDeckFor attrs c 1
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget _) _ -> pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) c cards -> do
      printed <- printedWillpower c
      when (any ((> printed) . fromMaybe 0 . cdThreat . toCardDef) cards) $ discard attrs c
      pure t
    _ -> DeepFissure <$> runMessage msg attrs
   where
    printedWillpower = \case
      HeroTarget h -> fromMaybe 0 . cdWillpower . toCardDef <$> getHero h
      AllyTarget a -> fromMaybe 0 . cdWillpower . toCardDef <$> getAlly a
      _ -> pure 0
