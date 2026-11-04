module LordOfTheRings.Quest.Cards.StormTheCastleNoQuarter (stormTheCastleNoQuarter) where

import LordOfTheRings.Card (Card (PlayerCard), HasCardDef, cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (runStormTheCastle)

newtype StormTheCastleNoQuarter = StormTheCastleNoQuarter QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest: Storm the Castle, No Quarter on its other side.
stormTheCastleNoQuarter :: QuestCard StormTheCastleNoQuarter
stormTheCastleNoQuarter = quest StormTheCastleNoQuarter Cards.stormTheCastleNoQuarter

{- | When Revealed: Each player randomly discards 4 of his cards from under this quest.
Then, each player discards each card he controls, or holds in his hand, that share a title
with one of the cards he just discarded: his heroes, allies and attachments among them.
-}
instance RunMessage StormTheCastleNoQuarter where
  runMessage msg q@(StormTheCastleNoQuarter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \p -> do
        mine <- shuffleM [pc | PlayerCard pc <- attrs.underneath, pc.owner == Just p]
        pure (p, take 4 mine)
      for_ discarded \(_, cards) -> traverse_ putInDiscard cards
      for_ discarded \(p, cards) -> do
        let discardedTitles = map (title . toCardDef) cards
            shares :: HasCardDef a => a -> Bool
            shares = (`elem` discardedTitles) . title . toCardDef
        hand <- (.hand) <$> getPlayer p
        for_ (filter shares hand) (discardFromHand p)
        heroes <- filter shares <$> (traverse getHero =<< select (HeroControlledBy $ PlayerWithId p))
        allies <- filter shares <$> (traverse getAlly =<< select (AllyControlledBy $ PlayerWithId p))
        attachments <- getsGame (filter (\a -> (toAttrs a).controller == p && shares a) . toList . entitiesAttachments . gameEntities)
        for_ heroes (discard . toTarget)
        for_ allies (discard . toTarget)
        for_ attachments (discard . toTarget)
      pure q
    _ -> StormTheCastleNoQuarter <$> runStormTheCastle msg attrs
   where
    title def = let Name t _ = cdName def in t
