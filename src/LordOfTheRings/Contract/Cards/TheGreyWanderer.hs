module LordOfTheRings.Contract.Cards.TheGreyWanderer (theGreyWanderer) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType), toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype TheGreyWanderer = TheGreyWanderer ContractAttrs
  deriving anyclass (IsContract, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Side A: You cannot choose more than 1 starting hero (checked as the deck is chosen,
-- 'LordOfTheRings.Decklist').
theGreyWanderer :: ContractCard TheGreyWanderer
theGreyWanderer = contract TheGreyWanderer Cards.theGreyWanderer

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
    -- Side B: The first non-unique card you play each planning phase does not require a
    -- resource match. (As the phase begins, before its action window.)
    CheckWindows ws | attrs.flipped, Window After (PhaseBegins PlanningPhase) `elem` ws -> do
      phaseModifier attrs (PlayerTarget attrs.owner) (NextIgnoresResourceMatch $ NotCard UniqueCard)
      pure c
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
