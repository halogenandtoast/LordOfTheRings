module LordOfTheRings.Treachery.Cards.UnseenDanger (unseenDanger) where

import LordOfTheRings.Card (cdThreat, toCard, toCardDef)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnseenDanger = UnseenDanger TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unseenDanger :: TreacheryCard UnseenDanger
unseenDanger = treachery UnseenDanger Cards.unseenDanger

{- | When Revealed: Each player removes a character he controls from the quest and discards
the top card of the encounter deck. If the discarded card's printed threat is greater than
the removed character's willpower, discard that character. A player with no character
committed still discards the card.
Shadow: Attacking enemy gets +2 attack and +2 defense if it is guarding a card.
-}
instance RunMessage UnseenDanger where
  runMessage msg t@(UnseenDanger attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        heroes <- map toTarget <$> select (CommittedHero <> HeroControlledBy (PlayerWithId p))
        allies <- map toTarget <$> select (CommittedAlly <> AllyControlledBy (PlayerWithId p))
        if null (heroes <> allies)
          then discardTopOfEncounterDeckFor attrs (PlayerTarget p) 1
          else chooseOneM p do
            questionLabeled "unseenDanger.remove"
            causedBy attrs
            targets (heroes <> allies) \c -> do
              removeFromQuest c
              discardTopOfEncounterDeckFor attrs c 1
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget _) _ -> pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) c cards -> do
      willpower <- getWillpower c
      when (any ((> willpower) . fromMaybe 0 . cdThreat . toCardDef) cards) $ discard attrs c
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      e <- getEnemy eid
      guarding <- guardingACard (toCard e).id (toTarget eid)
      when guarding do
        enemyGetsAttack attrs eid 2
        attackModifier attrs eid (DefenseModifier 2)
      pure t
    _ -> UnseenDanger <$> runMessage msg attrs
