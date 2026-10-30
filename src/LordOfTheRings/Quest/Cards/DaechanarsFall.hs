module LordOfTheRings.Quest.Cards.DaechanarsFall (daechanarsFall) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (cdCardType), toCardDef)
import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DaechanarsFall = DaechanarsFall QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

daechanarsFall :: QuestCard DaechanarsFall
daechanarsFall = quest DaechanarsFall Cards.daechanarsFall

-- | Forced: At the end of the quest phase, if there are any locations in play, deal 1
-- damage to all characters in play. If there are no locations in play, the players have
-- escaped Carn Dûm, and win the game.
instance HasAbilities DaechanarsFall where
  getAbilities (DaechanarsFall a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

{- | When Revealed: If there are less than X locations in the staging area, search the
encounter deck and discard pile and add locations to the staging area until there are X
locations in staging area. X is the number of players in the game.
Progress that would be placed on this quest is instead placed on locations in the staging
area, divided as you choose: the first player places it 1 at a time. With none there, it
is lost.
-}
instance RunMessage DaechanarsFall where
  runMessage msg q@(DaechanarsFall attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      DaechanarsFall <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      shuffleEncounterDeck
      pure q
    -- one location at a time, until there are X
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- length <$> getRemainingPlayers
      staged <- length <$> select LocationInStagingArea
      when (staged < x) do
        scenario <- getsGame (toAttrs . (.scenario))
        let isLocation = (== LocationType) . cdCardType . toCardDef
            inDeck = filter isLocation scenario.encounterDeck
            inDiscard = filter isLocation scenario.encounterDiscard
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "daechanarsFall.addLocation"
          causedBy attrs
          showing (map EncounterCard $ inDeck <> inDiscard)
          targets inDeck \card -> do
            removeFromEncounterDeck card
            addToStaging card
            doStep 1 msg
          targets inDiscard \card -> do
            addToStagingFromDiscard card
            doStep 1 msg
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress n | n > 0 -> do
      staged <- select LocationInStagingArea
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeledWith "daechanarsFall.placeProgress" [intVar "n" n]
        causedBy attrs
        targets staged \l -> do
          placeProgress source l 1
          placeProgress source attrs (n - 1)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      locations <- select AnyLocation
      if null locations
        then winGame
        else traverse_ (\c -> dealDamage attrs c 1) =<< selectCharacters AnyHero AnyAlly
      pure q
    _ -> DaechanarsFall <$> runMessage msg attrs
