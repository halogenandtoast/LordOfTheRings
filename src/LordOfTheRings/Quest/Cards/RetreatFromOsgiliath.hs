module LordOfTheRings.Quest.Cards.RetreatFromOsgiliath (retreatFromOsgiliath) where

import LordOfTheRings.Card (Card (PlayerCard), CardCost (StaticCost), CardType (AllyType), cdCardType, cdCost, cdName, cdUnique, toCardCode, toCardDef)
import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RetreatFromOsgiliath = RetreatFromOsgiliath QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

retreatFromOsgiliath :: QuestCard RetreatFromOsgiliath
retreatFromOsgiliath = quest RetreatFromOsgiliath Cards.retreatFromOsgiliath

-- | The allies in the staging area, under no player's control.
stagedAllies :: HasGame m => m [Ally]
stagedAllies = getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.allies))

-- | X is the total cost of each ally in the staging area.
retreatX :: HasGame m => m Int
retreatX = sum . map (\a -> case cdCost (toCardDef a) of Just (StaticCost n) -> n; _ -> 0) <$> stagedAllies

-- | X quest points. Allies in the staging area are treated as if their text box is blank,
-- are immune to player card effects, and are under no player's control (an ally placed in
-- the staging area has no controller).
instance HasModifiersFor RetreatFromOsgiliath where
  getModifiersFor (RetreatFromOsgiliath a) = when a.flipped do
    x <- retreatX
    modifySelf a [QuestPointModifier x]
    allies <- stagedAllies
    for_ allies \ally -> modified a ally.id [Blank, ImmuneToPlayerCardEffects]

-- | Forced: At the end of the round, assign damage among allies in the staging area equal
-- to the number of enemies in the staging area.
instance HasAbilities RetreatFromOsgiliath where
  getAbilities (RetreatFromOsgiliath a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed: Each player adds 1 set aside Wraith on Wings to the staging area and
reveals an encounter card. Starting with the first player, each player discards cards from
the top of his deck until he discards an ally that is not in play. Put the discarded ally
into play in the staging area and treat its text box as if it were blank until the end of
the phase. (An ally is in play while a character of its title is, by the unique rule; a
deck without one is discarded whole.)

The stage is defeated once its progress reaches X, which falls as the allies leave the
staging area.
-}
instance RunMessage RetreatFromOsgiliath where
  runMessage msg q@(RetreatFromOsgiliath attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RetreatFromOsgiliath <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n revealed@(WhenRevealed (isTarget attrs -> True)) | n < allyStep -> do
      players <- getRemainingPlayers
      case atMay players (n - 1) of
        Just p -> do
          wraiths <- getSetAside (CardWithCode $ toCardCode Enemies.wraithOnWings)
          for_ (headMay wraiths) addToStaging
          revealEncounterCardAs p
          doStep (n + 1) revealed
        Nothing -> doStep allyStep revealed
      pure q
    -- then the allies, starting with the first player
    DoStep n revealed@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - allyStep)) \p -> do
        deck <- (.deck) <$> getPlayer p
        inPlay <- titlesInPlay
        let allyNotInPlay = \case
              PlayerCard pc ->
                let def = toCardDef pc
                 in cdCardType def == AllyType && not (cdUnique def && title (cdName def) `elem` inPlay)
              _ -> False
        case break allyNotInPlay deck of
          (before, PlayerCard ally : _) -> do
            discardTopOfDeck p (length before + 1)
            removeFromDiscard p ally.id
            aid <- putAllyIntoStagingArea ally
            phaseModifier attrs (AllyTarget aid) Blank
          _ -> discardTopOfDeck p (length deck)
        doStep (n + 1) revealed
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      allies <- map (.id) <$> stagedAllies
      enemies <- length <$> select EnemyInStagingArea
      lead <- getLead
      when (notNull allies && enemies > 0) $ dealDamageDividedAmong attrs lead allies enemies
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> checkX =<< runMessage msg attrs
    CheckStage qid | qid == attrs.id -> checkX attrs
    -- an ally leaving the staging area lowers X
    Discard (AllyTarget _) -> do
      push $ CheckStage attrs.id
      pure q
    _ -> RetreatFromOsgiliath <$> runMessage msg attrs
   where
    -- the steps past the reveals, one for each player's ally
    allyStep = 10
    title (Name t _) = t
    titlesInPlay = do
      heroes <- getsGame (map (title . cdName . toCardDef) . toList . (.entities.heroes))
      allies <- getsGame (map (title . cdName . toCardDef) . toList . (.entities.allies))
      pure $ heroes <> allies
    checkX a = do
      x <- retreatX
      when (a.flipped && a.progress > 0 && a.progress >= x) $ push (CompleteQuest a.id)
      pure $ RetreatFromOsgiliath a
