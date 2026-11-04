module LordOfTheRings.Contract.Cards.TheGreyWanderer (theGreyWanderer) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType), toCardCode)
import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gamePhase, gameRound)
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype TheGreyWanderer = TheGreyWanderer ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Side A: You cannot choose more than 1 starting hero (checked as the deck is chosen,
-- 'LordOfTheRings.Decklist').
theGreyWanderer :: ContractCard TheGreyWanderer
theGreyWanderer = contract TheGreyWanderer Cards.theGreyWanderer

-- | The round whose planning phase saw the player's first non-unique card played, in the
-- meta.
spentIn :: ContractAttrs -> Maybe Int
spentIn a = parseMaybe parseJSON a.meta

{- | Side B: The first non-unique card you play each planning phase does not require a
resource match. Only during the planning phase, and only until a non-unique card has been
played in it, however it was paid for or played at no cost: that card is the first.
-}
instance HasModifiersFor TheGreyWanderer where
  getModifiersFor (TheGreyWanderer a) = when a.flipped do
    (phase, round') <- getsGame \g -> (gamePhase g, gameRound g)
    when (phase == PlanningPhase && spentIn a /= Just round') $
      modified a (PlayerTarget a.owner) [NextIgnoresResourceMatch $ NotCard UniqueCard]

-- | Side B: Action: Exhaust this card and raise your threat by 1 to (choose two): Ready
-- your starting hero, add 2 resources to your starting hero's resource pool, or heal 3
-- damage from your starting hero.
instance HasAbilities TheGreyWanderer where
  getAbilities (TheGreyWanderer a) =
    [ mkAbility a 1 $ ActionAbility $ Costs [exhaustCost a, RaiseThreatCost 1]
    | a.flipped
    , not a.exhausted
    ]

-- | The hero the player began the game with, while it is in play.
startingHero :: HasGame m => PlayerId -> m (Maybe HeroId)
startingHero pid = do
  started <- (.startingHeroes) <$> getPlayer pid
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
  pure $ headMay [h.id | h <- heroes, toCardCode h `elem` started]

instance RunMessage TheGreyWanderer where
  runMessage msg c@(TheGreyWanderer attrs) = case msg of
    -- Side A: Setup: Search your deck for an attachment with a printed cost of 1 and put it
    -- into play. Then, flip this card over. (A searched deck is shuffled after, FAQ 1.21.)
    DoStep 3 Setup | not attrs.flipped -> do
      let pid = attrs.owner
      deck <- (.deck) <$> getPlayer pid
      let found = [pc | PlayerCard pc <- deck, cardMatches (CardWithType AttachmentType <> CardWithPrintedCost 1) pc]
      chooseOneM pid do
        questionLabeled "theGreyWanderer.search"
        causedBy attrs
        showing (map PlayerCard found)
        for_ found \pc -> targeting (CardIdTarget pc.id) do
          removeFromDeck pid pc.id
          putAttachmentIntoPlay pid pc
      shufflePlayerDeck pid
      flipOver attrs
      pure c
    -- the first non-unique card played this planning phase, by any means, spends it
    CheckWindows ws
      | attrs.flipped
      , notNull [() | Window After (PlayedCard pid card) <- ws, pid == attrs.owner, not (cardMatches UniqueCard card)] -> do
          (phase, round') <- getsGame \g -> (gamePhase g, gameRound g)
          pure $ if phase == PlanningPhase then TheGreyWanderer attrs {contractMeta = toJSON round'} else c
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      startingHero pid >>= traverse_ \hero -> do
        let key :: Int -> Text
            key = \case
              1 -> "theGreyWanderer.ready"
              2 -> "theGreyWanderer.resources"
              _ -> "theGreyWanderer.heal"
            effect :: ReverseQueue n => Int -> n ()
            effect = \case
              1 -> ready (HeroTarget hero)
              2 -> placeResources attrs (HeroTarget hero) 2
              _ -> healDamage attrs (HeroTarget hero) 3
            -- each option chosen resolves, then the next is chosen from those left
            choose :: ReverseQueue n => Int -> [Int] -> n ()
            choose n opts = when (n > 0) $ chooseOneM pid do
              questionLabeled "theGreyWanderer.chooseTwo"
              causedBy attrs
              for_ opts \i -> labeled (key i) do
                effect i
                choose (n - 1) (filter (/= i) opts)
        choose 2 [1, 2, 3]
      pure c
    _ -> TheGreyWanderer <$> runMessage msg attrs
