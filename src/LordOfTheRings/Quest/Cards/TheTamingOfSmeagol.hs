module LordOfTheRings.Quest.Cards.TheTamingOfSmeagol (theTamingOfSmeagol) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (cdThreat), CardType (LocationType), genEncounterCard, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (firstPlayerControlsSmeagol)

newtype TheTamingOfSmeagol = TheTamingOfSmeagol QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTamingOfSmeagol :: QuestCard TheTamingOfSmeagol
theTamingOfSmeagol = quest TheTamingOfSmeagol Cards.theTamingOfSmeagol

gollum :: EnemyMatcher
gollum = EnemyWithCardCode (toCardCode Enemies.gollumTheLandOfShadow)

-- | Forced: After a player declares any number of attackers against Gollum, discard the top
-- card of the encounter deck. Gollum gets +X defense for this attack, where X is the
-- discarded card's threat.
instance HasAbilities TheTamingOfSmeagol where
  getAbilities (TheTamingOfSmeagol a) = [mkAbility a 1 $ forced $ MatchingAttackersDeclaredWindow After gollum | a.flipped]

{- | Setup: Put Gollum into play engaged with the first player. Each player searches the
encounter deck for 1 location, reveals it, and adds it to the staging area. Shuffle the
encounter deck. One player after another, each searching the deck as the one before left
it.

The players cannot advance unless the first player controls Sméagol.
-}
instance RunMessage TheTamingOfSmeagol where
  runMessage msg q@(TheTamingOfSmeagol attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      putEnemyIntoPlayEngaged lead =<< genEncounterCard Enemies.gollumTheLandOfShadow
      doStep 1 msg
      TheTamingOfSmeagol <$> runMessage msg attrs
    DoStep n inner@(PerformSetup (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          let locations = filter (cardMatches $ CardWithType LocationType) deck
          chooseOrRunOneM pid do
            questionLabeled "theTamingOfSmeagol.search"
            showing (map EncounterCard locations)
            for_ locations \c -> targeting (CardIdTarget c.id) do
              removeFromEncounterDeck c
              push $ RevealCard c
          doStep (n + 1) inner
        [] -> shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      gollums <- select gollum
      let x = sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- cards]
      when (x > 0) $ for_ gollums \g -> attackModifier attrs g (DefenseModifier x)
      pure q
    -- Sméagol may come under the first player's control as Gollum turns over or the token
    -- passes
    Flip _ -> recheck
    SetFirstPlayer _ -> recheck
    _ -> TheTamingOfSmeagol <$> runConditionalStage firstPlayerControlsSmeagol msg attrs
   where
    recheck = do
      push $ CheckStage attrs.id
      TheTamingOfSmeagol <$> runConditionalStage firstPlayerControlsSmeagol msg attrs
