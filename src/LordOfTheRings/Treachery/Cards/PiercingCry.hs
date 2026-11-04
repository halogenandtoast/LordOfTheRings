module LordOfTheRings.Treachery.Cards.PiercingCry (piercingCry) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), EncounterCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PiercingCry = PiercingCry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
piercingCry :: TreacheryCard PiercingCry
piercingCry = treachery PiercingCry Cards.piercingCry

{- | When Revealed: Search the encounter deck and discard pile for a Nazgûl enemy. Either add
it to the staging area or put it into play engaged with the first player. Shuffle the
encounter deck. The player who revealed it chooses, on his own (Peril).
Shadow: Attacking enemy makes an additional attack after this one.
-}
instance RunMessage PiercingCry where
  runMessage msg t@(PiercingCry attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      lead <- getLead
      (deck, pile) <- getsGame \g -> let s = toAttrs g.scenario in (s.encounterDeck, s.encounterDiscard)
      let nazgul = filter (cardMatches $ CardWithTrait Nazgul <> CardWithType EnemyType)
          place :: ReverseQueue m => EncounterCard -> m ()
          place c = chooseOneM you do
            questionLabeled "piercingCry.place"
            causedBy attrs
            showing [EncounterCard c]
            labeled "piercingCry.staging" $ addToStaging c
            labeled "piercingCry.engaged" $ putEnemyIntoPlayEngaged lead c
      chooseOrRunOneM you do
        questionLabeled "piercingCry.search"
        causedBy attrs
        showing (map EncounterCard $ nazgul deck <> nazgul pile)
        for_ (nazgul deck) \c -> targeting (CardIdTarget c.id) $ removeFromEncounterDeck c >> place c
        for_ (nazgul pile) \c -> targeting (CardIdTarget c.id) $ removeFromEncounterDiscard c >> place c
      shuffleEncounterDeck
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> PiercingCry <$> runMessage msg attrs
