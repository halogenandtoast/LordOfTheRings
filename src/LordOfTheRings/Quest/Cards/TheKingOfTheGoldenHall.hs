module LordOfTheRings.Quest.Cards.TheKingOfTheGoldenHall (theKingOfTheGoldenHall) where

import LordOfTheRings.Card (CardType (AllyType), cdSphere, genEncounterCard, toCardDef)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheKingOfTheGoldenHall = TheKingOfTheGoldenHall QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingOfTheGoldenHall :: QuestCard TheKingOfTheGoldenHall
theKingOfTheGoldenHall = quest TheKingOfTheGoldenHall Cards.theKingOfTheGoldenHall

-- | Skip the quest phase and combat phase.
instance HasModifiersFor TheKingOfTheGoldenHall where
  getModifiersFor (TheKingOfTheGoldenHall a) = when a.flipped $ modified a GameTarget [SkipQuestPhase, SkipPhase CombatPhase]

-- | Forced: At the end of the round, advance to stage 2A.
instance HasAbilities TheKingOfTheGoldenHall where
  getAbilities (TheKingOfTheGoldenHall a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Set The Defense of Helm's Deep and 1 copy of Deeping Wall aside, out of play.
Each player sets 1 copy of Soldier of Isengard aside, out of play (the scenario does as it
gathers). Then, the players must choose: either each player shuffles 1 copy of Poisoned
Counsels into his deck, or advance to stage 2A (bypassing stage 1B). The first player
makes the group's choice.

When Revealed (side B): Each player chooses 1 ally in his hand that matches the sphere of
a hero he controls and puts it into play under his control.
-}
instance RunMessage TheKingOfTheGoldenHall where
  runMessage msg q@(TheKingOfTheGoldenHall attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "theKingOfTheGoldenHall.choose"
        labeled "theKingOfTheGoldenHall.poisonedCounsels" do
          eachPlayer \p -> shuffleIntoDeck p =<< genEncounterCard Treacheries.poisonedCounselsTheTreasonOfSaruman
          turnOver attrs
        labeled "theKingOfTheGoldenHall.advance" advanceQuest
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheKingOfTheGoldenHall <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        player <- getPlayer p
        candidates <- filterM passesUnique [c | c <- player.hand, c.cardType == AllyType]
        allies <- filterM (matchesAHero p) candidates
        chooseOrRunOneM p do
          questionLabeled "theKingOfTheGoldenHall.chooseAlly"
          causedBy attrs
          targets allies \c -> putAllyIntoPlayFromHand p c.id
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      advanceQuest
      pure q
    _ -> TheKingOfTheGoldenHall <$> runMessage msg attrs
   where
    matchesAHero p c = case cdSphere (toCardDef c) of
      Just s -> selectAny (HeroControlledBy (PlayerWithId p) <> HeroWithSphere s)
      Nothing -> pure False
